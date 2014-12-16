{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# OPTIONS_GHC -fno-warn-orphans  #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.Builder
-- Copyright   :  (c) 2012 diagrams-lib team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- Tools for dynamically building diagrams, for /e.g./ creating
-- preprocessors to interpret diagrams code embedded in documents.
--
-----------------------------------------------------------------------------
module Diagrams.Builder
       ( -- * Building diagrams

         -- ** Options
         BuildOpts(..), mkBuildOpts, backendOpts, snippets, pragmas, imports, decideRegen, diaExpr, postProcess

         -- ** Regeneration decision functions and hashing
       , alwaysRegenerate, hashedRegenerate
       , hashToHexStr

         -- ** Building
       , buildDiagram, BuildResult(..)
       , ppInterpError

         -- * Interpreting diagrams
         -- $interp
       , setDiagramImports
       , interpretDiagram

         -- * Tools for creating standalone builder executables

       , Build(..)
       , defaultBuildOpts

       ) where

import           Control.Lens                        ((^.))
import           Control.Monad                       (guard, mplus, mzero)
import           Control.Monad.Catch                 (catchAll)
import           Control.Monad.Trans.Maybe           (MaybeT, runMaybeT)
import           Data.Data
import           Data.Hashable                       (Hashable (..))
import           Data.List                           (foldl', isSuffixOf, nub)
import           Data.List.Split                     (splitOn)
import           Data.Maybe                          (catMaybes, fromMaybe)
import           System.Directory                    (doesFileExist,
                                                      getTemporaryDirectory,
                                                      removeFile)
import           System.FilePath                     (takeBaseName, (<.>),
                                                      (</>))
import           System.IO                           (hClose, hPutStr,
                                                      openTempFile)

import           Language.Haskell.Exts               (ImportDecl, Module (..),
                                                      importModule, prettyPrint)
import           Language.Haskell.Interpreter        hiding (ModuleName)

import           Diagrams.Builder.CmdLine
import           Diagrams.Builder.Modules
import           Diagrams.Builder.Opts
import           Diagrams.Prelude
import           Language.Haskell.Interpreter.Unsafe (unsafeRunInterpreterWithArgs)
import           System.Directory (getDirectoryContents)
import           System.Environment                  (getEnvironment)
import           System.FilePath (splitDirectories, splitExtensions)

deriving instance Typeable Any

------------------------------------------------------------
-- Interpreting diagrams
------------------------------------------------------------

-- $interp
-- These functions constitute the internals of diagrams-builder.  End
-- users should not usually need to call them directly; use
-- 'buildDiagram' instead.

-- | Set up the module to be interpreted, in the context of the
--   necessary imports.
setDiagramImports
  :: MonadInterpreter m
  => String
     -- ^ Filename of the module containing the diagrams

  -> [String]
     -- ^ Additional necessary imports. @Prelude@, @Diagrams.Prelude@,
     --   @Diagrams.Core.Types@, and @Data.Monoid@ are included by
     --   default.

  -> m ()

setDiagramImports m imps = do
    loadModules [m]
    setTopLevelModules [takeBaseName m]
    setImports $ [ "Prelude"
                 , "Diagrams.Prelude"
                 , "Diagrams.Core.Types"
                 , "Data.Monoid"
                 ]
                 ++ imps

-- | Make it easier to specify a package database in a sandbox by
-- looking for a lone package-db if only the sandbox directory is
-- given. If no package DB is found beneath the given directory, the
-- argument is returned unchanged.
findSandbox :: FilePath -> IO FilePath
findSandbox d
    | last (splitDirectories d) == ".cabal-sandbox" =
        do dbs <- filter isDB `fmap` getDirectoryContents d
           case dbs of
             [] -> return d
             [db] -> return $ d </> db
             _ -> error "Found package databases for multiple GHCs. \
                         \\Set the DIAGRAMS_SANDBOX environment variable\
                         \to a specific one."
    | otherwise = putStrLn "Not a sandbox?" >> return d
  where isDB = (".conf.d" `isSuffixOf`) . snd . splitExtensions

getHsenvArgv :: IO [String]
getHsenvArgv = do
  env <- getEnvironment
  case lookup "HSENV" env of
    Nothing -> maybe (return [])
                     (fmap (pure . ("-package-db "++)) . findSandbox)
                     (lookup "DIAGRAMS_SANDBOX" env)
    _ -> return . words $ fromMaybe "" (lookup "PACKAGE_DB_FOR_GHC" env)

-- | Interpret a diagram expression based on the contents of a given
--   source file, using some backend to produce a result.  The
--   expression can be of type @Diagram b v n@ or @IO (Diagram b v n)@.
interpretDiagram
  :: forall b v n.
     ( Typeable b
#if __GLASGOW_HASKELL__ > 707
     , Typeable v
#else
     , Typeable1 v
#endif
     , HasLinearMap v, Data (v n), Data n
     , Metric v, OrderedField n, Backend b v n
     )
  => BuildOpts b v n
  -> FilePath
  -> IO (Either InterpreterError (Result b v n))
interpretDiagram bopts m = do

  -- use an hsenv sandbox, if one is enabled.
  args <- liftIO getHsenvArgv
  unsafeRunInterpreterWithArgs args $ do

    setDiagramImports m (bopts ^. imports)
    let dexp = bopts ^. diaExpr

    -- Try interpreting the diagram expression at two types: Diagram
    -- b v and IO (Diagram b v).  Take whichever one typechecks,
    -- running the IO action in the second case to produce a
    -- diagram.
    d <- interpret dexp (as :: QDiagram b v n Any) `catchAll` const (interpret dexp (as :: IO (QDiagram b v n Any)) >>= liftIO)

    -- Finally, call renderDia.
    return $ renderDia (backendToken bopts) (bopts ^. backendOpts) ((bopts ^. postProcess) d)

-- | Pretty-print an @InterpreterError@.
ppInterpError :: InterpreterError -> String
ppInterpError (UnknownError err) = "UnknownError: " ++ err
ppInterpError (WontCompile  es)  = unlines . nub . map errMsg $ es
ppInterpError (NotAllowed   err) = "NotAllowed: "   ++ err
ppInterpError (GhcException err) = "GhcException: " ++ err

------------------------------------------------------------
-- Build a diagram using a temporary file
------------------------------------------------------------

-- | Potential results of a dynamic diagram building operation.
data BuildResult b v n =
    ParseErr  String           -- ^ Parsing of the code failed.
  | InterpErr InterpreterError -- ^ Interpreting the code
                               --   failed. See 'ppInterpError'.
  | Skipped Hash               -- ^ This diagram did not need to be
                               --   regenerated; includes the hash.
  | OK Hash (Result b v n)     -- ^ A successful build, yielding the
                               --   hash and a backend-specific result.

-- | Build a diagram by writing the given source code to a temporary
--   module and interpreting the given expression, which can be of
--   type @Diagram b v@ or @IO (Diagram b v)@.  Can return either a
--   parse error if the source does not parse, an interpreter error,
--   or the final result.
buildDiagram
  :: ( Typeable b, Data (v n), Data n
     , Metric v, HasLinearMap v
#if __GLASGOW_HASKELL__ > 707
     , Typeable v
#else
     , Typeable1 v
#endif
     , OrderedField n, Backend b v n
     , Hashable (Options b v n)
     )
  => BuildOpts b v n -> IO (BuildResult b v n)
buildDiagram bopts = do
  let bopts' = bopts
             & snippets %~ map unLit
             & pragmas  %~ ("NoMonomorphismRestriction" :)
             & imports  %~ ("Diagrams.Prelude" :)
  case createModule Nothing bopts' of
    Left  err -> return (ParseErr err)
    Right m@(Module _ _ _ _ _ srcImps _) -> do
      liHash <- hashLocalImports srcImps
      let diaHash
            = 0 `hashWithSalt` prettyPrint m
                `hashWithSalt` (bopts ^. diaExpr)
                `hashWithSalt` (bopts ^. backendOpts)
                `hashWithSalt` liHash
      regen    <- (bopts ^. decideRegen) diaHash
      case regen of
        Nothing  -> return $ Skipped diaHash
        Just upd -> do
          tmpDir   <- getTemporaryDirectory
          (tmp, h) <- openTempFile tmpDir "Diagram.hs"
          let m' = replaceModuleName (takeBaseName tmp) m
          hPutStr h (prettyPrint m')
          hClose h

          compilation <- interpretDiagram (bopts' & backendOpts %~ upd) tmp
          removeFile tmp
          return $ either InterpErr (OK diaHash) compilation

-- | Take a list of imports, and return a hash of the contents of
--   those imports which are local.  Note, this only finds imports
--   which exist relative to the current directory, which is not as
--   general as it probably should be --- we could be calling
--   'buildDiagram' on source code which lives anywhere.
hashLocalImports :: [ImportDecl] -> IO Hash
hashLocalImports
  = fmap (foldl' hashWithSalt 0 . catMaybes)
  . mapM (getLocalSource . foldr1 (</>) . splitOn "." . getModuleName . importModule)

-- | Given a relative path with no extension, like
--   @\"Foo\/Bar\/Baz\"@, check whether such a file exists with either
--   a @.hs@ or @.lhs@ extension; if so, return its /pretty-printed/
--   contents (removing all comments, canonicalizing formatting, /etc./).
getLocalSource :: FilePath -> IO (Maybe String)
getLocalSource f = runMaybeT $ do
  contents <- getLocal f
  case (doModuleParse . unLit) contents of
    Left _  -> mzero
    Right m -> return (prettyPrint m)

-- | Given a relative path with no extension, like
--   @\"Foo\/Bar\/Baz\"@, check whether such a file exists with either a
--   @.hs@ or @.lhs@ extension; if so, return its contents.
getLocal :: FilePath -> MaybeT IO String
getLocal m = tryExt "hs" `mplus` tryExt "lhs"
  where
    tryExt ext = do
      let f = m <.> ext
      liftIO (doesFileExist f) >>= guard >> liftIO (readFile f)
