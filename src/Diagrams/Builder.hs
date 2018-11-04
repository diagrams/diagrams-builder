{-# LANGUAGE CPP                   #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveFoldable        #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DeriveTraversable     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MonoLocalBinds        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeFamilies          #-}

{-# OPTIONS_GHC -fno-warn-orphans        #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

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
    diaSnippet
  , saveDiagramExpression
  , snippetInterpret
  , runInterpret
  , runSandboxInterpreter
  , cacheRun
  , cacheRunI
  , DiagramBuilder (..)
  , ppInterpError
  ) where

import           System.Directory

import           Control.Lens                        (cons, (^.))
import           Control.Monad
import           Control.Monad.Catch
import           Data.Hashable                       (Hashable (..))
import           Data.List                           (nub)
import           Data.Typeable
import           Data.Word                           (Word)
import           Numeric                             (showHex)
import           System.Directory                    (copyFile, doesFileExist)
import           System.FilePath                     (takeBaseName,
                                                      takeExtension, (</>))
import           System.IO                           (IOMode (WriteMode),
                                                      hClose, hPutStr, withFile)
import           System.IO.Temp

import           Diagrams.Backend

import           Diagrams.Builder.Modules
import           Diagrams.Builder.Opts
import           Diagrams.Prelude                    hiding (dir)

import           Language.Haskell.Exts.Simple
import           Language.Haskell.Interpreter        hiding (ModuleName)
import           Language.Haskell.Interpreter.Unsafe

-- There are multiple ways to use the diagrams-builds package. In
-- principle the majority of this package is not limited to
-- diagrams, but it has been written with diagrams in mind.
--
-- There are multiple levels at which you can use the diagrams-builder.
-- The most common way is 'builderWithCache' that takes an expression

-- | An encapsulation of code that should interpret to the value @a@.
data Interpret a = Interpret
  { interpretModule  :: Module
  , interpretImports :: [(String, Maybe String)]
  , interpExpr       :: String
  }

instance Typeable a => Hashable (Interpret a) where
  hashWithSalt s i@Interpret {..} =
    s `hashWithSalt` prettyPrint interpretModule
      `hashWithSalt` interpretImports
      `hashWithSalt` interpExpr
      `hashWithSalt` typeRep i

-- | Information about how save a diagram.
data DiagramBuilder = DiagramBuilder
  { _diaInfo    :: BackendInfo
  , _diaExpr    :: String
  , _diaOutSize :: SizeSpec V2 Int
  }

-- | A diagram builder snippet that saves to a file.
diaSnippet
  :: Snippets
  -- ^ snippets to import
  -> DiagramBuilder
  -- ^ expression to run
  -> Either String (Interpret (FilePath -> IO ()))
  -- ^ either parse error or interpreter
diaSnippet snips builder = snippetInterpret snips' expr
  where
    expr = saveDiagramExpression (_diaExpr builder) (_diaInfo builder) (_diaOutSize builder)
    snips' = snips & imports <>~ backendModules (_diaInfo builder)

-- | The literal expression that can be interpreted to save a diagram.
saveDiagramExpression :: String -> BackendInfo -> SizeSpec V2 Int -> String
saveDiagramExpression expr info sz = "(\\saveDiagramExpressionPath -> " ++ unwords
  [ "saveDiagram"
  , backendTokenName info
  , "saveDiagramExpressionPath"
  , showSizeSpec sz
  , parens expr
  ] ++ ")"

-- | Make an interpret out of a snippet and the expression to evaluate.
snippetInterpret
  :: Snippets
  -- ^ snippets to import
  -> String
  -- ^ expression to run
  -> Either String (Interpret a)
snippetInterpret snip expr = do
  let snip' = prepareSnippet snip
      imps = map (,Nothing) (snip' ^. imports) ++
             map (_2 %~ Just) (snip' ^. qimports)
  modu <- createModule Nothing snip'
  pure $ Interpret
    { interpretModule  = modu
    , interpretImports = imps
    , interpExpr    = expr
    }

-- | Run an interpret.
runInterpret
  :: (MonadInterpreter m, Typeable a)
  => Interpret a
  -> m a
runInterpret Interpret {..} =
  tempModule interpretModule $ \path -> do
    loadModules [path]
    setTopLevelModules [takeBaseName path]
    setImportsQ interpretImports
    interpretExpr interpExpr

-- | Call a function that writes to a file via a cache. If the hash
--   already existed do not call the function and instead copy the file
--   from the cache.
cache
  :: MonadIO m
  => FilePath
  -- ^ cache path
  -> Hash
  -- ^ the hash assosiated with a thing
  -> (FilePath -> m ())
  -- ^ function to write to a file
  -> FilePath
  -- ^ path to write file to
  -> m Bool
  -- ^ whether the hash already existed
cache cacheDir h f outputPath = do
  liftIO $ createDirectoryIfMissing True cacheDir
  let hashPath = cacheDir </> showHash h <> takeExtension outputPath
  hashExists <- liftIO $ doesFileExist hashPath
  unless hashExists $ f hashPath
  liftIO $ copyFile hashPath outputPath
  pure hashExists

-- | Run an file saving interpret that uses a cache to copy files that
--   have already been computed (using the hash of the 'Interpret').
--   Prints an error if there was an interpretor error.
cacheRun
  :: FilePath
  -- ^ cache path
  -> Interpret (FilePath -> IO ())
  -- ^ result in a function the writes to a file
  -> FilePath
  -- ^ target destination
  -> IO (Either InterpreterError Bool)
  -- ^ whether the hash already existed
cacheRun cacheDir i outputPath = try $
  cache cacheDir (hash i) func outputPath
  where
    func path = do
      a <- runSandboxInterpreter $ do
        f <- runInterpret i
        liftIO $ f path
      case a of
        Left ierr -> throwM ierr -- putStrLn $ ppInterpError ierr
        Right x   -> pure x

-- | Similar to 'cacheRun' but runs in a 'MonadInterpreter'.
cacheRunI
  :: MonadInterpreter m
  => FilePath
  -- ^ cache path
  -> Interpret (FilePath -> IO ())
  -- ^ result in a function the writes to a file
  -> FilePath
  -- ^ target destination
  -> m (Either InterpreterError Bool)
  -- ^ whether the hash already existed
cacheRunI cacheDir i outputPath = try $
  cache cacheDir (hash i) func outputPath
  where
    func path = do
      f <- runInterpret i
      liftIO $ f path

-- | Show a size spec in a way that can be parsed by ghc.
showSizeSpec :: SizeSpec V2 Int -> String
showSizeSpec sz = case getSpec sz of
  -- note that getSpec only return positive values so we don't need to
  -- bracket x or y
  V2 (Just x) (Just y) -> "(dims2D " ++ show x ++ " " ++ show y ++ ")"
  V2 (Just x) Nothing  -> "(mkWidth " ++ show x ++ ")"
  V2 Nothing (Just y)  -> "(mkHeight " ++ show y ++ ")"
  V2 Nothing Nothing   -> "absolute"

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
-- Interpreting
------------------------------------------------------------

-- | Interpret a @Diagram@ or @IO Diagram@ at the name of the function.
--   (This means the source should already be loaded by the
--   interpreter.)
interpretExpr
  :: forall m a. (MonadInterpreter m, Typeable a)
  => String
  -> m a
interpretExpr expr =
  interpret expr (as :: a) `catchAll`
  const (interpret expr (as :: IO a) >>= liftIO)

-- | Write a module to a temporary file and delete it when done. The
--   module name is replaced by the temporary file's name (\"Build\").
tempModule :: (MonadIO m, MonadMask m) => Module -> (FilePath -> m a) -> m a
tempModule m f =
  withSystemTempDirectory "builder" $ \tempDir -> do
    let tempFile = tempDir </> "Build.hs"
    liftIO $ withFile tempFile WriteMode $ \h -> do
      let m' = replaceModuleName "Build" m
      liftIO $ hPutStr h (prettyPrint m') >> hClose h
    f tempFile

-- | Run an interpretor using sandbox from 'findSandbox'.
runSandboxInterpreter
  :: (MonadMask m, MonadIO m)
  => InterpreterT m a
  -> m (Either InterpreterError a)
runSandboxInterpreter i = do
  mSandbox <- liftIO $ findSandbox []
  case mSandbox of
    Just sandbox -> let args = ["-package-db", sandbox]
                    in  unsafeRunInterpreterWithArgs args i
    Nothing      -> runInterpreter i

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

prepareSnippet :: Snippets -> Snippets
prepareSnippet o = o &~ do
  snippets %= map unLit
  pragmas  %= cons "NoMonomorphismRestriction"
  imports  %= cons "Diagrams.Prelude"
