{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TemplateHaskell       #-}
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
         BuildOpts(..), backendOpts, snippets, pragmas, imports, decideRegen, diaExpr, postProcess

         -- ** Building
       , buildDiagram, BuildResult(..)
       , ppInterpError

         -- ** Regeneration decision functions
       , alwaysRegenerate, hashedRegenerate

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
import           Control.Monad.Error                 (catchError)
import           Control.Monad.Trans.Maybe           (MaybeT, runMaybeT)
import           Crypto.Hash                         (Digest, MD5,
                                                      digestToHexByteString,
                                                      hash)
import qualified Data.ByteString.Char8               as B
import           Data.List                           (nub)
import           Data.List.Split                     (splitOn)
import           Data.Maybe                          (catMaybes, fromMaybe)
import           Data.Typeable                       (Typeable)
import           System.Directory                    (doesFileExist,
                                                      getDirectoryContents,
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
import           Diagrams.Prelude                    hiding ((<.>))
import           Language.Haskell.Interpreter.Unsafe (unsafeRunInterpreterWithArgs)
import           System.Environment                  (getEnvironment)

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

getHsenvArgv :: IO [String]
getHsenvArgv = do
  env <- getEnvironment
  return $ case (lookup "HSENV" env) of
             Nothing -> []
             _       -> hsenvArgv
                 where hsenvArgv = words $ fromMaybe "" (lookup "PACKAGE_DB_FOR_GHC" env)

-- | Interpret a diagram expression based on the contents of a given
--   source file, using some backend to produce a result.  The
--   expression can be of type @Diagram b v@ or @IO (Diagram b v)@.
interpretDiagram
  :: forall b v x.
     ( Typeable b, Typeable v
     , InnerSpace v, OrderedField (Scalar v), Backend b v
     )
  => BuildOpts b v x
  -> FilePath
  -> IO (Either InterpreterError (Result b v))
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
    d <- interpret dexp (as :: Diagram b v) `catchError` const (interpret dexp (as :: IO (Diagram b v)) >>= liftIO)

    -- Finally, call renderDia.
    return $ renderDia (backendToken bopts) (bopts ^. backendOpts) d

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
data BuildResult b v x =
    ParseErr  String              -- ^ Parsing of the code failed.
  | InterpErr InterpreterError    -- ^ Interpreting the code
                                  --   failed. See 'ppInterpError'.
  | Skipped x                     -- ^ This diagram did not need to be
                                  --   regenerated.
  | OK x (Result b v)             -- ^ A successful build, yielding a
                                  --   backend-specific result and
                                  --   some extra information.

-- | Build a diagram by writing the given source code to a temporary
--   module and interpreting the given expression, which can be of
--   type @Diagram b v@ or @IO (Diagram b v)@.  Can return either a
--   parse error if the source does not parse, an interpreter error,
--   or the final result.
buildDiagram
  :: ( Typeable b, Typeable v
     , InnerSpace v, OrderedField (Scalar v), Backend b v
     , Show (Options b v)
     )
  => BuildOpts b v x -> IO (BuildResult b v x)
buildDiagram bopts = do
  let bopts' = bopts
             & snippets %~ map unLit
             & pragmas  %~ ("NoMonomorphismRestriction" :)
             & imports  %~ ("Diagrams.Prelude" :)
  case createModule Nothing bopts' of
    Left  err -> return (ParseErr err)
    Right m@(Module _ _ _ _ _ srcImps _) -> do
      liHashes <- getLocalImportHashes srcImps
      regen    <- (bopts ^. decideRegen) (prettyPrint m ++ (bopts ^. diaExpr) ++ show (bopts ^. backendOpts) ++ concat liHashes)
      case regen of
        (info, Nothing)  -> return $ Skipped info
        (info, Just upd) -> do
          tmpDir   <- getTemporaryDirectory
          (tmp, h) <- openTempFile tmpDir ("Diagram.hs")
          let m' = replaceModuleName (takeBaseName tmp) m
          hPutStr h (prettyPrint m')
          hClose h

          compilation <- interpretDiagram (bopts' & backendOpts %~ upd) tmp
          removeFile tmp
          return $ either InterpErr (OK info) compilation

-- | Take a list of imports, and return hashes of the contents of
--   those imports which are local.  Note, this only finds imports
--   which exist relative to the current directory, which is not as
--   general as it probably should be --- we could be calling
--   'buildDiagram' on source code which lives anywhere.
getLocalImportHashes :: [ImportDecl] -> IO [String]
getLocalImportHashes
  = (fmap . map) hashStr
  . fmap catMaybes
  . mapM getLocalSource
  . map (foldr1 (</>) . splitOn "." . getModuleName . importModule)

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

-- | Convenience function suitable to be given as the final argument
--   to 'buildDiagram'.  It implements the simple policy of always
--   rebuilding every diagram.
alwaysRegenerate :: String -> IO ((), Maybe (a -> a))
alwaysRegenerate _ = return ((), Just id)

-- | Convenience function suitable to be given as the final argument
--   to 'buildDiagram'.  It works by hashing the given diagram source,
--   and looking in the specified directory for any file whose base
--   name is equal to the hash.  If there is such a file, it specifies
--   that the diagram should not be rebuilt.  Otherwise, it specifies
--   that the diagram should be rebuilt, and uses the provided
--   function to update the rendering options based on the generated
--   hash.  (Most likely, one would want to set the requested output
--   file to the hash followed by some extension.)  It also returns
--   the generated hash.
hashedRegenerate
  :: (String -> a -> a)
     -- ^ A function for computing an update to rendering options,
     --   given a new base filename computed from a hash of the
     --   diagram source.

  -> FilePath
     -- ^ The directory in which to look for generated files

  -> String
     -- ^ The \"source\" to hash. Note that this does not actually
     -- have to be valid source code.  A common trick is to
     -- concatenate the actual source code with String representations
     -- of any other information on which the diagram depends.

  -> IO (String, Maybe (a -> a))

hashedRegenerate upd d src = do
  let fileBase = hashStr src
  files <- getDirectoryContents d
  case any ((fileBase==) . takeBaseName) files of
    True  -> return (fileBase, Nothing)
    False -> return (fileBase, Just (upd fileBase))

hashStr :: String -> String
hashStr = B.unpack . digestToHexByteString . (hash :: B.ByteString -> Digest MD5) . B.pack
