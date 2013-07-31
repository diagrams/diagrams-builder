{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

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

         buildDiagram, BuildResult(..)
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

import           Control.Monad                (guard, mplus, mzero)
import           Control.Monad.Trans.Maybe    (MaybeT, runMaybeT)
import           Crypto.Hash                  (Digest, MD5,
                                               digestToHexByteString, hash)
import qualified Data.ByteString.Char8        as B
import           Data.List                    (nub)
import           Data.List.Split              (splitOn)
import           Data.Maybe                   (catMaybes, fromMaybe)
import           Data.Typeable                (Typeable)
import           System.Directory             (doesFileExist,
                                               getDirectoryContents,
                                               getTemporaryDirectory,
                                               removeFile)
import           System.FilePath              (takeBaseName, (<.>), (</>))
import           System.IO                    (hClose, hPutStr, openTempFile)

import           Language.Haskell.Exts        (ImportDecl, Module (..),
                                               ModuleName (..), importModule,
                                               prettyPrint)
import           Language.Haskell.Interpreter hiding (ModuleName)

import           Diagrams.Builder.CmdLine
import           Diagrams.Builder.Modules
import           Diagrams.Prelude             hiding (e, (<.>))
import           System.Environment           (getEnvironment)
import           Language.Haskell.Interpreter.Unsafe (unsafeRunInterpreterWithArgs)

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
--   source file, using some backend to produce a result.
interpretDiagram
  :: forall b v.
     ( Typeable b, Typeable v
     , InnerSpace v, OrderedField (Scalar v), Backend b v
     )
  => b             -- ^ Backend token
  -> v             -- ^ Dummy vector to identify the vector space
  -> Options b v   -- ^ Rendering options
  -> FilePath      -- ^ Filename of the module containing the example
  -> [String]      -- ^ Additional imports needed
  -> String        -- ^ Expression of type @Diagram b v@ to be compiled
  -> IO (Either InterpreterError (Result b v))
interpretDiagram b _ opts m imps dexp = do
    args <- liftIO getHsenvArgv
    unsafeRunInterpreterWithArgs args $ do
      setDiagramImports m imps
      d <- interpret dexp (as :: Diagram b v)
      return (renderDia b opts d)

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
--   module and interpreting the given expression.  Can return either
--   a parse error if the source does not parse, an interpreter error,
--   or the final result.
buildDiagram
  :: ( Typeable b, Typeable v
     , InnerSpace v, OrderedField (Scalar v), Backend b v
     , Show (Options b v)
     )
  => b
     -- ^ Backend token

  -> v
     -- ^ Dummy vector to fix the vector type

  -> Options b v
     -- ^ Backend-specific options to use

  -> [String]
     -- ^ Source code snippets.  Each should be a syntactically valid
     --   Haskell module.  They will be combined intelligently, /i.e./
     --   not just pasted together textually but combining pragmas,
     --   imports, /etc./ separately.

  -> String
     -- ^ Diagram expression to interpret

  -> [String]
     -- ^ Extra @LANGUAGE@ pragmas to use (@NoMonomorphismRestriction@
     --   is used by default.)

  -> [String]
     -- ^ Additional imports ("Diagrams.Prelude" is imported by
     --   default).

  -> (String -> IO (x, Maybe (Options b v -> Options b v)))
     -- ^ A function to decide whether a particular diagram needs to
     --   be regenerated.  It will be passed the final assembled
     --   source for the diagram (but with the module name set to
     --   @Main@ instead of something auto-generated, so that hashing
     --   the source will produce consistent results across runs). It
     --   can return some information (such as a hash of the source)
     --   via the @x@ result, which will be passed through to the
     --   result of 'buildDiagram'.  More importantly, it decides
     --   whether the diagram should be built: a result of 'Just'
     --   means the diagram /should/ be built; 'Nothing' means it
     --   should not. In the case that it should be built, it returns
     --   a function for updating the rendering options.  This could
     --   be used, /e.g./, to request a filename based on a hash of
     --   the source.
     --
     --   Two standard decision functions are provided for
     --   convenience: 'alwaysRegenerate' returns no extra information
     --   and always decides to regenerate the diagram;
     --   'hashedRegenerate' creates a hash of the diagram source and
     --   looks for a file with that name in a given directory.

  -> IO (BuildResult b v x)
buildDiagram b v opts source dexp langs imps shouldRegen = do
  let source'   = map unLit source
  case createModule
         Nothing
         ("NoMonomorphismRestriction" : langs)
         ("Diagrams.Prelude" : imps)
         source' of
    Left  err -> return (ParseErr err)
    Right m@(Module _ _ _ _ _ srcImps _) -> do
      liHashes <- getLocalImportHashes srcImps
      regen    <- shouldRegen (prettyPrint m ++ dexp ++ show opts ++ concat liHashes)
      case regen of
        (info, Nothing)  -> return $ Skipped info
        (info, Just upd) -> do
          tmpDir   <- getTemporaryDirectory
          (tmp, h) <- openTempFile tmpDir ("Diagram.hs")
          let m' = replaceModuleName (takeBaseName tmp) m
          hPutStr h (prettyPrint m')
          hClose h

          compilation <- interpretDiagram b v (upd opts) tmp imps dexp
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

hashedRegenerate upd dir src = do
  let fileBase = hashStr src
  files <- getDirectoryContents dir
  case any ((fileBase==) . takeBaseName) files of
    True  -> return (fileBase, Nothing)
    False -> return (fileBase, Just (upd fileBase))

hashStr :: String -> String
hashStr = B.unpack . digestToHexByteString . (hash :: B.ByteString -> Digest MD5) . B.pack
