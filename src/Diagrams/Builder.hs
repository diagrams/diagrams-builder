{-# LANGUAGE CPP                   #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveFoldable        #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DeriveTraversable     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MonoLocalBinds        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeFamilies          #-}
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
module Diagrams.Builder where
  -- ( -- * Building diagrams

  --   -- ** Options
  --   BuildOpts (..)

  --   -- *** Lenses
  -- , mkBuildOpts
  -- , backendOpts
  -- , snippets
  -- , pragmas
  -- , imports
  -- , diaExpr
  -- , postProcess
  -- , hashCache

  --   -- ** Building
  -- , BuildResult (..)
  -- , buildDia
  -- , buildDiaResult
  -- , buildDiaToFile
  -- , buildDiaToHash
  -- , ppInterpError
  -- , showHash

  --   -- * Interpreting diagrams
  --   -- $interp
  -- , setDiaImports
  -- , interpretDia

  --   -- * Type aliases
  -- , Backend', BackendBuild', Hash

  -- ) where

import           Control.Applicative
import           Control.Lens                        (Traversal', cons, (^.),
                                                      (^?), _Just)
import           Control.Monad
import           Control.Monad.Catch
import           Control.Monad.Trans.Maybe           (MaybeT, runMaybeT)
import           Data.Foldable                       (Foldable)
import           Data.Hashable                       (Hashable (..))
import           Data.List                           (find, foldl', nub)
import           Data.List.Split                     (splitOn)
import           Data.Maybe
import           Data.Traversable                    as T (Traversable, mapM)
import           Data.Typeable
import           Data.Word                           (Word)
import           Numeric                             (showHex)
import           System.Directory                    (copyFile, doesFileExist,
                                                      getDirectoryContents)
import           System.FilePath                     (takeBaseName,
                                                      takeExtension, (<.>),
                                                      (</>))
import           System.IO                           (IOMode (WriteMode),
                                                      hClose, hPutStr, withFile)
import           System.IO.Temp

import           Diagrams.Backend

import           Diagrams.Builder.Modules
import           Diagrams.Builder.Opts
import           Diagrams.Prelude

import           Language.Haskell.Exts.Simple
import           Language.Haskell.Interpreter        hiding (ModuleName)
import           Language.Haskell.Interpreter.Unsafe

-- Type synonyms for saner type signatures.

type BackendBuild' b =
  (BackendBuild b, Hashable (Options b), Typeable b,
   Typeable (V b), HasLinearMap (V b), Metric (V b))

type Backend' b = (Typeable b, Typeable (V b), HasLinearMap (V b), Metric (V b), Backend b)

diaBuildOpts :: BackendBuild' b => Options b -> BuildOpts (Diagram (V b))
diaBuildOpts opts = BuildOpts
  { -- _postProcess = id
    _hashCache   = Nothing
  , _buildSave = flip saveDiagram' opts
  -- , _buildHash   = hash opts
  , _buildExpr   = "diagram"
  }

saveDiaBuilder :: Snippet -> DiaBuildOpts -> IO (BuildResult ())
saveDiaBuilder snip diaOpts = do
  let snip' = prepareSnippet $ snip & imports <>~ diaOpts^.buildInfo.to backendModules
  let expr = saveExpr (diaOpts^.diaBuildExpr)
                      (diaOpts^.buildInfo)
                      (diaOpts^.diaOutputSize)
                      (diaOpts^.buildTarget)
      diaHash = 12314 -- XXX
  case createModule Nothing snip' of
    Left err -> return (ParseError err)
    Right m  -> do
      -- diaHash <- hashModule bopts m
      let diaHash = 123453 -- XXX

      let imps = map (,Nothing) (snip' ^. imports) ++
                 map (_2 %~ Just) (snip' ^. qimports)
          -- expr = bopts ^. buildExpr
          buildDia = do
            d <- runSandboxInterpreter $ interpretFromModule m imps expr
            return $ either InterpError (OK diaHash) d

      case diaOpts ^. cachePath of
        Nothing   -> buildDia
        Just path -> do
          alreadyDone <- isJust <$> checkHash path diaHash
          if alreadyDone
            then return $ Skipped diaHash
            else buildDia

showSizeSpec :: SizeSpec V2 Int -> String
showSizeSpec sz = case getSpec sz of
  -- note that getSpec only return positive values so we don't need to
  -- bracket x or y
  V2 (Just x) (Just y) -> "(dims2D " ++ show x ++ " " ++ show y ++ ")"
  V2 (Just x) Nothing  -> "(mkWidth " ++ show x ++ ")"
  V2 Nothing (Just y)  -> "(mkHeight " ++ show y ++ ")"
  V2 Nothing Nothing   -> "absolute"

saveExpr :: String -> BackendInfo -> SizeSpec V2 Int -> FilePath -> String
saveExpr expr info sz path = unwords
  [ "saveDiagram"
  , backendTokenName info
  , show path
  , showSizeSpec sz
  , parens expr
  ]



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
--   module name is replaced by the temporary file's name (\"Diagram\").
tempModule :: (MonadIO m, MonadMask m) => Module -> (FilePath -> m a) -> m a
tempModule m f =
  withSystemTempDirectory "builder" $ \tempDir -> do
    let tempFile = tempDir </> "Build.hs"
    liftIO $ withFile tempFile WriteMode $ \h -> do
      let m' = replaceModuleName "Build" m
      liftIO $ hPutStr h (prettyPrint m') >> hClose h
    f tempFile

-- | Same as 'interpretDiaWithOpts' but save 'Module' to a temporary file
--   and import it.
interpretFromModule
  :: (MonadInterpreter m, Typeable a)
  => Module
  -> [(String, Maybe String)]
  -> String
  -> m a
interpretFromModule m imps expr =
  tempModule m $ \path -> do
    loadModules [path]
    setTopLevelModules [takeBaseName path]
    setImportsQ imps
    interpretExpr expr

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

-- | Build a diagram. If the build hash is found, skip interpreting.
buildResult
  :: Typeable a
  => Snippet
  -> BuildOpts a
  -> IO (BuildResult a)
buildResult (prepareSnippet -> snip) bopts =
  case createModule Nothing snip of
    Left err -> return (ParseError err)
    Right m  -> do
      diaHash <- hashModule bopts m

      let imps = map (,Nothing) (snip ^. imports) ++
                 map (_2 %~ Just) (snip ^. qimports)
          expr = bopts ^. buildExpr
          getDia fullHash = do
            d <- runSandboxInterpreter $ interpretFromModule m imps expr
            return $ either InterpError (OK fullHash) d

      case bopts ^. hashCache of
        Nothing             -> getDia diaHash
        Just (optHash,path) -> do
          let fullHash = optHash `hashWithSalt` diaHash
          alreadyDone <- isJust <$> checkHash path fullHash
          if alreadyDone
            then return $ Skipped fullHash
            else getDia fullHash

-- | Build a diagram and save it to the given 'FilePath'. The
--   'hashCache' is used if it is present.
saveResult
  :: Typeable a
  => Snippet
  -> BuildOpts a
  -> FilePath
  -> (FilePath -> a -> IO ())
  -> IO (BuildResult a)
saveResult snip bopts outFile save = do
  let ext = takeExtension outFile
  case bopts ^. hashCache of
    Just (optHash,dir) -> do
      r <- buildResult snip bopts -- saveToHash snip bopts save (takeExtension outFile)
      case r ^? resultHash of
        Nothing -> return r
        Just h  -> do
          -- save (dir </> showHash h <.> ext) r
          copyFile (dir </> showHash h <.> ext) outFile
                >> return r

    Nothing -> do
      r <- buildResult snip bopts
      case r of
        OK h a -> save outFile a
               >> return r
        _      -> return r

-- buildDiaResult
--   :: BackendBuild' b
--   => BuildOpts b
--   -> IO (BuildResult (Result b))
-- buildDiaResult opts = do
--   d <- buildDia opts
--   return $ diaResult opts <$> d

------------------------------------------------------------------------
-- Hashing
------------------------------------------------------------------------

-- | Make a hash from BuildOpts and the Module. The hash includes any
--   local imports the module has.
hashModule :: BuildOpts a -> Module -> IO Hash
hashModule bopts m@(Module _ _ srcImps _) = do
  liHash <- hashLocalImports srcImps
  return (0 `hashWithSalt` prettyPrint m
            -- `hashWithSalt` (bopts ^. buildHash)
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
getLocal m = tryExt "hs" <|> tryExt "lhs"
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

prepareSnippet :: Snippet -> Snippet
prepareSnippet o = o &~ do
  snippets %= map unLit
  pragmas  %= cons "NoMonomorphismRestriction"
  imports  %= cons "Diagrams.Prelude"

