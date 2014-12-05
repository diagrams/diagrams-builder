{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveFoldable        #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DeriveTraversable     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE ViewPatterns          #-}
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
module Diagrams.Builder where
       -- ( -- * Building diagrams

       --   -- ** Options
       --   BuildOpts(..), mkBuildOpts, backendOpts, snippets, pragmas, imports,
       --   diaExpr, postProcess

       --   -- ** Regeneration decision functions and hashing
       -- -- , alwaysRegenerate, hashedRegenerate
       -- -- , hashToHexStr

       --   -- ** Building
       -- , buildDiagram, BuildResult(..)
       -- , ppInterpError

       --   -- * Interpreting diagrams
       --   -- $interp
       -- , setDiagramImports
       -- , interpretDiagram

       --   -- * Tools for creating standalone builder executables

       -- , Build(..)
       -- , defaultBuildOpts

       -- ) where

import           Control.Lens                 (cons, (^.))
import           Control.Monad
import           Control.Monad.Catch
import           Control.Monad.Trans.Maybe    (MaybeT, runMaybeT)
import           Data.Data
import           Data.Foldable                (Foldable)
import           Data.Hashable                (Hashable (..))
import           Data.List                    (foldl', nub)
import           Data.List                    (find)
import           Data.List.Split              (splitOn)
import           Data.Maybe
import           Data.Traversable             as T (Traversable, mapM)
import           Data.Word                    (Word)
import           Numeric                      (showHex)
import           System.Directory             (doesFileExist)
import           System.Directory             (getDirectoryContents)
import           System.FilePath              (takeBaseName, (<.>), (</>))
import           System.IO                    (hClose, hPutStr)
import           System.IO.Temp

import           Diagrams.Backend.Build
import           Diagrams.Builder.Modules
import           Diagrams.Builder.Opts
import           Diagrams.Prelude

import           Language.Haskell.Exts        (ImportDecl, Module (..), importModule, prettyPrint)
import           Language.Haskell.Interpreter hiding (ModuleName)

deriving instance Typeable Any

-- Typeable1 is a depreciated synonym in ghc > 707
#if __GLASGOW_HASKELL__ >= 707
#define Typeable1 Typeable
#endif

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

-- standardImports :: [ModuleName]
-- standardImports =
--   [ "Prelude"
--   , "Diagrams.Prelude"
--   , "Diagrams.Core.Types"
--   , "Data.Monoid"
--   ]

-- getHsenvArgv :: IO [String]
-- getHsenvArgv = do
--   env <- getEnvironment
--   return $ case lookup "HSENV" env of
--     Nothing -> []
--     _       -> hsenvArgv
--         where hsenvArgv = words $ fromMaybe "" (lookup "PACKAGE_DB_FOR_GHC" env)

interpretDia
  :: (MonadInterpreter m, Typeable (QDiagram b v n Any))
  => BuildOpts b v n
  -> FilePath
  -> m (QDiagram b v n Any)
interpretDia bopts m = do
  setDiagramImports m (bopts ^. imports)
  interpDiagram (bopts ^. diaExpr)

interpretResult
  :: ( MonadInterpreter m
     , Typeable b, Typeable1 v, HasLinearMap v, Metric v
     , Typeable n, OrderedField n, Backend b v n)
  => BuildOpts b v n
  -> FilePath
  -> m (Result b v n)
interpretResult bopts m = f `liftM` interpretDia bopts m
  where
    f d = renderDia (backendToken bopts) (bopts ^. backendOpts) ((bopts ^. postProcess) d)

interpretBuild
  :: (MonadInterpreter m, BackendBuild b v n r
     , Typeable b, Typeable1 v, HasLinearMap v, Metric v
     , Typeable n, OrderedField n
     )
  => BuildOpts b v n
  -> FilePath -- ^ path to Module
  -> r
  -> FilePath -- ^ path to save diagram
  -> m (Maybe String)
interpretBuild bopts m r outF = interpretDia bopts m >>= liftIO . f
  where
    f = buildDia' r outF (bopts ^. backendOpts)

-- | Interpret a @Diagram@ or @IO Diagram@ at the name of the function.
interpDiagram :: forall m b v n m'. (MonadInterpreter m, Typeable (QDiagram b v n m'))
              => String -> m (QDiagram b v n m')
interpDiagram dExp =
  interpret dExp (as :: QDiagram b v n m') `catchAll`
  const (interpret dExp (as :: IO (QDiagram b v n m')) >>= liftIO)

------------------------------------------------------------------------
-- Build a diagram using a temporary file
------------------------------------------------------------------------

-- | Potential results of a dynamic diagram building operation.
data BuildResult r
  = ParseError String           -- ^ Parsing of the code failed.
  | InterpError InterpreterError -- ^ Interpreting the code
                               --   failed. See 'ppInterpError'.
  | Skipped Hash               -- ^ This diagram did not need to be
                               --   regenerated; includes the hash.
  | OK Hash r                  -- ^ A successful build
  deriving (Show, Functor, Foldable, Traversable)

buildResult
  :: (MonadInterpreter m, BackendBuild b v n r, Hashable (Options b v n )
     , Typeable b, Typeable1 v, HasLinearMap v, Metric v
     , Typeable n, OrderedField n
     )
  => BuildOpts b v n
  -> r
  -> FilePath
  -> m (BuildResult (Maybe String))
buildResult opts r outF = do
  d <- buildDiagram opts
  liftIO $ buildDia' r outF (opts ^. backendOpts) `T.mapM` d

buildDiagram
  :: (MonadInterpreter m, Typeable (QDiagram b v n Any), Hashable (Options b v n))
  => BuildOpts b v n
  -> m (BuildResult (QDiagram b v n Any))
buildDiagram (prepareOpts -> bopts) = case createModule Nothing bopts of
  Left err -> return (ParseError err)
  Right m  -> do
    diaHash <- liftIO $ hashModule bopts m
    case bopts ^. hashCache of
      Nothing   -> OK diaHash `liftM` buildDiagram' bopts m
      Just path -> do
        alreadyDone <- liftIO $ isJust <$> checkHash path diaHash
        if alreadyDone
          then return $ Skipped diaHash
          else OK diaHash `liftM` buildDiagram' bopts m

buildDiagram'
  :: (MonadInterpreter m, Typeable (QDiagram b v n Any))
  => BuildOpts b v n
  -> Module
  -> m (QDiagram b v n Any)
buildDiagram' bopts m = tempModule m (interpretDia bopts)

-- | Write a module to a tempory file and delete it when done. The
--   module name is replaced by the tempory file's name (\"Diagram\").
tempModule :: (MonadIO m, MonadMask m) => Module -> (FilePath -> m a) -> m a
tempModule m f =
  withSystemTempFile "Diagram.hs" $ \temp h -> do
    let m' = replaceModuleName (takeBaseName temp) m
    liftIO $ hPutStr h (prettyPrint m') >> hClose h
    liftIO $ putStrLn $ prettyPrint m'
    f temp

-- | Check for an existing rendered diagram in the directory that
--   matches the hash.
checkHash :: FilePath -> Hash -> IO (Maybe FilePath)
checkHash dir diaHash = do
  files <- getDirectoryContents dir
  return $ find ((== hashHex diaHash) . takeBaseName) files

prepareOpts :: BuildOpts b v n -> BuildOpts b v n
prepareOpts o = o & snippets %~ map unLit
                  & pragmas  %~ cons "NoMonomorphismRestriction"
                  & imports  %~ cons "Diagrams.Prelude"

------------------------------------------------------------------------
-- Hashing
------------------------------------------------------------------------

-- | Make a hash from BuildOpts and the Module. The hash includes any
--   local imports the module has.
hashModule :: Hashable (Options b v n) => BuildOpts b v n -> Module -> IO Hash
hashModule bopts m@(Module _ _ _ _ _ srcImps _) = do
  liHash <- hashLocalImports srcImps
  return (0 `hashWithSalt` prettyPrint m
            `hashWithSalt` (bopts ^. diaExpr)
            `hashWithSalt` (bopts ^. backendOpts)
            `hashWithSalt` liHash)

------------------------
-- Hashing local imports
------------------------

-- We hash local imports in case they've changed.

-- | Take a list of imports, and return a hash of the contents of
--   those imports which are local.  Note, this only finds imports
--   which exist relative to the current directory, which is not as
--   general as it probably should be --- we could be calling
--   'buildDiagram' on source code which lives anywhere.
hashLocalImports :: [ImportDecl] -> IO Hash
hashLocalImports
  = fmap (foldl' hashWithSalt 0 . catMaybes)
  . T.mapM (getLocalSource . foldr1 (</>) . splitOn "." . getModuleName . importModule)

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

------------------------------------------------------------------------
-- Utilities
------------------------------------------------------------------------

-- | Pretty-print an @InterpreterError@.
ppInterpError :: InterpreterError -> String
ppInterpError (UnknownError err) = "UnknownError: " ++ err
ppInterpError (WontCompile  es)  = unlines . nub . map errMsg $ es
ppInterpError (NotAllowed   err) = "NotAllowed: "   ++ err
ppInterpError (GhcException err) = "GhcException: " ++ err

hashHex :: Hash -> String
hashHex h = showHex (fromIntegral h :: Word) ""
