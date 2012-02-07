{-# LANGUAGE StandaloneDeriving
           , DeriveDataTypeable
           , ScopedTypeVariables
           , FlexibleContexts
  #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Diagrams.Builder where

import Diagrams.Prelude hiding ((<.>))

import Language.Haskell.Interpreter

import System.IO
import System.FilePath
import System.Directory

import Data.List (intercalate, isPrefixOf)

import Data.Typeable
deriving instance Typeable Any

-- | Set up the module to be interpreted, in the context of the
--   necessary imports.
setDiagramImports :: MonadInterpreter m
                  => String      -- ^ Filename of the module containing the diagrams
                  -> [String]    -- ^ Additional necessary imports
                  -> m ()
setDiagramImports m imps = do
    loadModules [m]
    setTopLevelModules [takeBaseName m]
    setImports $ [ "Prelude"
                 , "Diagrams.Prelude"
                 , "Graphics.Rendering.Diagrams.Core"
                 , "Data.Monoid"
                 ]
                 ++ imps

-- | Interpret a diagram expression based on the contents of a given
--   source file, using some backend to produce a result.
interpretDiagram :: forall b v.
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
interpretDiagram b _ opts m imps dexp =
    runInterpreter $ do
      setDiagramImports m imps
      d <- interpret dexp (as :: Diagram b v)
      return (renderDia b opts d)

ppError :: InterpreterError -> IO ()
ppError (UnknownError err) = putStrLn $ "UnknownError: " ++ err
ppError (WontCompile  es)  = putStr . unlines . map errMsg $ es
ppError (NotAllowed   err) = putStrLn $ "NotAllowed: "   ++ err
ppError (GhcException err) = putStrLn $ "GhcException: " ++ err

-- | Generate a file header for a module containing diagram definitions.
diagramFileHeader :: String       -- ^ Module name to use.
                  -> Bool         -- ^ True if the module should use bird tracks.
                  -> [String]     -- ^ @LANGUAGE@ pragmas to use.
                                  --   @NoMonomorphismRestriction@ is
                                  --   included by default.
                  -> [String]     -- ^ Extra modules to import.
                                  --   "Diagrams.Prelude" is imported by
                                  --   default.
                  -> String
diagramFileHeader modName bird langs imps
  = unlines . birdize
  $ [ languagePragma
    , "module " ++ modName ++ " where"
    , "import Diagrams.Prelude"
    ] ++ extraImports
  where
    languagePragma = "{-# LANGUAGE NoMonomorphismRestriction, "
                  ++ intercalate ", " langs
                  ++ "#-}"
    extraImports = map ("import " ++) imps
    birdize
      | bird      = map ("> "++)
      | otherwise = id

-- | Create a module with some given diagrams code in a temporary file.
genTempDiagramModule :: String      -- ^ Source code
                     -> [String]    -- ^ Extra @LANGUAGE@ pragmas to
                                    --   use
                                    --   (@NoMonomorphismRestriction@
                                    --   is used by default.)
                     -> [String]    -- ^ Additional imports
                                    --   ("Diagrams.Prelude" is imported by
                                    --   default).
                     -> IO FilePath
genTempDiagramModule source langs imps = do
  let useBirdTracks = any (">" `isPrefixOf`) (lines source)
      ext | useBirdTracks = "lhs"
          | otherwise     = "hs"
  tmpDir <- getTemporaryDirectory
  (tmp, h) <- openTempFile tmpDir ("Diagram" <.> ext)
  hPutStr h (diagramFileHeader
              (takeBaseName tmp)
              useBirdTracks
              langs
              imps
            )
  hPutStr h source
  hClose h
  return tmp

-- | Build a diagram by writing the given source code to a temporary
--   module and interpreting the given expression.
buildDiagram :: ( Typeable b, Typeable v
                , InnerSpace v, OrderedField (Scalar v), Backend b v
                )
             => b              -- ^ Backend token
             -> v              -- ^ Dummy vector to fix the vector type
             -> Options b v    -- ^ Backend-specific options to use
             -> String         -- ^ Source code
             -> String         -- ^ Diagram expression to interpret
             -> [String]       -- ^ Extra @LANGUAGE@ pragmas to use
                               --   (@NoMonomorphismRestriction@ is used
                               --   by default.)
             -> [String]       -- ^ Additional imports
                               --   ("Diagrams.Prelude" is imported by
                               --   default).
             -> IO (Either InterpreterError (Result b v))
buildDiagram b v opts source dexp langs imps = do
  tmp <- genTempDiagramModule source langs imps
  compilation <- interpretDiagram b v opts tmp imps dexp
  removeFile tmp
  return compilation
