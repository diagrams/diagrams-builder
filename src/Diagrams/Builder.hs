{-# LANGUAGE CPP                   #-}
{-# LANGUAGE ConstraintKinds       #-}
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
-- Copyright   :  (c) 2012-2015 diagrams-lib team (see LICENSE)
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
    BuildOpts (..)

    -- *** Lenses
  , mkBuildOpts
  , backendOpts
  , snippets
  , pragmas
  , imports
  , diaExpr
  , postProcess
  , hashCache

    -- ** Building
  , BuildResult (..)
  , buildDia
  , buildDiaResult
  , buildDiaToFile
  , buildDiaToHash
  , ppInterpError
  , showHash

    -- * Interpreting diagrams
    -- $interp
  , setDiaImports
  , interpretDia

    -- * Type aliases
  , Backend', BackendBuild', Hash

  ) where

import           Control.Lens                 (Traversal', cons, (^.), (^?),
                                               _Just)
import           Control.Monad
import           Control.Monad.Catch
import           Control.Monad.Trans.Maybe    (MaybeT, runMaybeT)
import           Data.Foldable                (Foldable)
import           Data.Hashable                (Hashable (..))
import           Data.List                    (find, foldl', nub)
import           Data.List.Split              (splitOn)
import           Data.Maybe
import           Data.Traversable             as T (Traversable, mapM)
import           Data.Typeable
import           Data.Word                    (Word)
import           Numeric                      (showHex)
import           System.Directory             (copyFile, doesFileExist,
                                               getDirectoryContents)
import           System.FilePath              (takeBaseName, takeExtension,
                                               (<.>), (</>))
import           System.IO                    (hClose, hPutStr)
import           System.IO.Temp

import           Diagrams.Backend.Build
import           Diagrams.Builder.Modules
import           Diagrams.Builder.Opts
import           Diagrams.Prelude

import           Language.Haskell.Exts        (ImportDecl, Module (..),
                                               importModule, prettyPrint)
import           Language.Haskell.Interpreter hiding (ModuleName)
import           Language.Haskell.Interpreter.Unsafe

deriving instance Typeable Any

-- Typeable1 is a depreciated synonym in ghc > 707
#if __GLASGOW_HASKELL__ >= 707
#define Typeable1 Typeable
#endif

-- Type synonyms for saner type signatures.

type BackendBuild' b v n =
  (BackendBuild b v n, Hashable (Options b v n), Typeable b, Typeable1 v,
   HasLinearMap v, Metric v, Typeable n, OrderedField n)

type Backend' b v n =
  (Typeable b, Typeable1 v, HasLinearMap v, Metric v,
   Typeable n, OrderedField n, Backend b v n)

-- | Synonym for more perspicuous types.
--
--   We use @Int@ values for hashes because that's what the @Hashable@
--   package uses.  Assuming diagram hashes are uniformly distributed,
--   on a 64-bit system one needs to build on the order of billions of
--   diagrams before the probability of a hash collision exceeds 1/2,
--   and for anything up to tens of millions of diagrams the
--   probability of a collision is under 0.1%.  On 32-bit systems
--   those become tens of thousands and thousands, respectively.
type Hash = Int

------------------------------------------------------------
-- Interpreting diagrams
------------------------------------------------------------

-- $interp
-- These functions constitute the internals of diagrams-builder.  End
-- users should not usually need to call them directly; use
-- 'buildDia' instead.

-- | Set up the module to be interpreted, in the context of the
--   necessary imports.
setDiaImports
  :: MonadInterpreter m
  => String   -- ^ Filename of the module containing the diagrams
  -> [String] -- ^ Additional necessary imports. @Prelude@ and
              --   @Diagrams.Prelude@ are included by default.
  -> m ()

setDiaImports m imps = do
  loadModules [m]
  setTopLevelModules [takeBaseName m]
  setImports $ [ "Prelude"
               , "Diagrams.Prelude"
               ]
               ++ imps

-- | Interpret the module, set imports from 'BuildOpts' and return the
--   'Diagram' with the 'postProcess' applied.
interpretDiaWithOpts
  :: (MonadInterpreter m, Typeable (QDiagram b v n Any))
  => BuildOpts b v n
  -> FilePath
  -> m (QDiagram b v n Any)
interpretDiaWithOpts bopts m = do
  setDiaImports m (bopts ^. imports)
  (bopts ^. postProcess) `liftM` interpretDia (bopts ^. diaExpr)

-- | Same as 'interpretDiaWithOpts' but save 'Module' to a temporary file
--   and import it.
interpretDiaModule
  :: (MonadInterpreter m, Typeable (QDiagram b v n Any))
  => BuildOpts b v n
  -> Module
  -> m (QDiagram b v n Any)
interpretDiaModule bopts m = tempModule m (interpretDiaWithOpts bopts)

-- | Interpret a @Diagram@ or @IO Diagram@ at the name of the function.
--   (This means the source should already be loaded by the
--   interpreter.)
interpretDia
  :: forall m b v n. (MonadInterpreter m, Typeable (QDiagram b v n Any))
  => String -> m (QDiagram b v n Any)
interpretDia dExp =
  interpret dExp (as :: QDiagram b v n Any) `catchAll`
  const (interpret dExp (as :: IO (QDiagram b v n Any)) >>= liftIO)

-- | Convenient function to turn a 'QDiagram' to its 'Result' using
--   'BuildOpts'. The 'postProcess' is not applied.
diaResult :: Backend' b v n => BuildOpts b v n -> QDiagram b v n Any -> Result b v n
diaResult bopts = renderDia (backendToken bopts) (bopts ^. backendOpts)

------------------------------------------------------------------------
-- Build a diagram using a temporary file
------------------------------------------------------------------------

-- | Potential results of a dynamic diagram building operation.
data BuildResult r
  = ParseError String            -- ^ Parsing of the code failed.
  | InterpError InterpreterError -- ^ Interpreting the code
                                 --   failed. See 'ppInterpError'.
  | Skipped Hash                 -- ^ This diagram did not need to be
                                 --   regenerated; includes the hash.
  | OK Hash r                    -- ^ A successful build
  deriving (Show, Functor, Foldable, Traversable)

-- | Traversal over the 'Hash' of a 'BuildResult' if no error occurred.
resultHash :: Traversal' (BuildResult r) Hash
resultHash f (Skipped h) = Skipped <$> f h
resultHash f (OK h r)    = OK <$> f h <*> pure r
resultHash _ err         = pure err

-- | Build a diagram and save it to it's hash. If no directory is
--   specified for the hash use the current directory.
buildDiaToHash
  :: BackendBuild' b v n
  => BuildOpts b v n
  -> String          -- ^ extension
  -> IO (BuildResult ())
buildDiaToHash opts ext = do
  let dir = opts ^. hashCache . _Just
  d <- buildDia opts
  case d of
    OK h dia -> saveDia (dir </> showHash h <.> ext) (opts ^. backendOpts) dia
             >> return (OK h ())
    _        -> return (() <$ d)

-- | Build a diagram and save it to the given 'FilePath'. The
--   'hashCache' is used if it is present.
buildDiaToFile
  :: BackendBuild' b v n
  => BuildOpts b v n
  -> FilePath
  -> IO (BuildResult ())
buildDiaToFile bopts outFile = do
  let ext = takeExtension outFile
  case bopts ^. hashCache of
    Just dir -> do
      r <- buildDiaToHash bopts (takeExtension outFile)
      case r ^? resultHash of
        Just h  -> copyFile (dir </> showHash h <.> ext) outFile
                >> return r
        Nothing -> return r

    Nothing -> do
      d <- buildDia bopts
      case d of
        OK h dia -> saveDia outFile (bopts ^. backendOpts) dia
                 >> return (OK h ())
        _        -> return (() <$ d)

buildDiaResult
  :: BackendBuild' b v n
  => BuildOpts b v n
  -> IO (BuildResult (Result b v n))
buildDiaResult opts = do
  d <- buildDia opts
  return $ diaResult opts <$> d

-- | Build a diagram. If the module hash is found, skip interpreting.
buildDia
  :: (Hashable (Options b v n), Typeable b, Typeable1 v, Typeable n)
  => BuildOpts b v n
  -> IO (BuildResult (QDiagram b v n Any))
buildDia (prepareOpts -> bopts) = case createModule Nothing bopts of
  Left err -> return (ParseError err)
  Right m  -> do
    diaHash <- hashModule bopts m

    let getDia = do
          d <- runSandboxInterpreter $ interpretDiaModule bopts m
          return $ either InterpError (OK diaHash) d

    case bopts ^. hashCache of
      Nothing   -> getDia
      Just path -> do
        alreadyDone <- isJust <$> checkHash path diaHash
        if alreadyDone
          then return $ Skipped diaHash
          else getDia

-- | Run an interpretor using sandbox from 'findSandbox'.
runSandboxInterpreter :: (MonadMask m, MonadIO m, Functor m)
                      => InterpreterT m a -> m (Either InterpreterError a)
runSandboxInterpreter i = do
  mSandbox <- liftIO $ findSandbox []
  case mSandbox of
    Just sandbox -> let args = ["-package-db", sandbox]
                    in  unsafeRunInterpreterWithArgs args i
    Nothing      -> runInterpreter i

-- | Write a module to a temporary file and delete it when done. The
--   module name is replaced by the temporary file's name (\"Diagram\").
tempModule :: (MonadIO m, MonadMask m) => Module -> (FilePath -> m a) -> m a
tempModule m f =
  withSystemTempFile "Diagram.hs" $ \temp h -> do
    let m' = replaceModuleName (takeBaseName temp) m
    liftIO $ hPutStr h (prettyPrint m') >> hClose h
    f temp

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
--   'buildDia' on source code which lives anywhere.
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
ppInterpError (WontCompile  es)  = unlines . nub $ map errMsg es
ppInterpError (NotAllowed   err) = "NotAllowed: "   ++ err
ppInterpError (GhcException err) = "GhcException: " ++ err

-- Turn a Hash into a hex with no leading 0x. Hash is converted to a
-- word to avoid negative values.
showHash :: Hash -> String
showHash h = showHex (fromIntegral h :: Word) ""

-- | Check for an existing rendered diagram in the directory that
--   matches the hash.
checkHash :: FilePath -> Hash -> IO (Maybe FilePath)
checkHash dir diaHash = do
  files <- getDirectoryContents dir
  return $ find ((== showHash diaHash) . takeBaseName) files

prepareOpts :: BuildOpts b v n -> BuildOpts b v n
prepareOpts o = o & snippets %~ map unLit
                  & pragmas  %~ cons "NoMonomorphismRestriction"
                  & imports  %~ cons "Diagrams.Prelude"
