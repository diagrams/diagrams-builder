{-# LANGUAGE StandaloneDeriving
           , DeriveDataTypeable
           , ScopedTypeVariables
           , FlexibleContexts
  #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Diagrams.Builder where

import Diagrams.Prelude

import Language.Haskell.Interpreter

import System.FilePath

import Data.List (intercalate)

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

-- | Compile a diagram expression based on the contents of a given
--   source file, using some backend to produce a result.
compileDiagram :: forall b v. 
                  ( Typeable b, Typeable v
                  , InnerSpace v, OrderedField (Scalar v), Backend b v
                  )
               => b             -- ^ Backend token
               -> v             -- ^ Dummy vector to identify the vector space
               -> Options b v   -- ^ Rendering options
               -> String        -- ^ Filename of the module containing the example
               -> [String]      -- ^ Additional imports needed
               -> String        -- ^ Expression of type @Diagram b v@ to be compiled
               -> IO (Either InterpreterError (Result b v))
compileDiagram b _ opts m imps dexp =
    runInterpreter $ do
      setDiagramImports m imps
      d <- interpret dexp (as :: Diagram b v)
      return (renderDia b opts d)

{-
ppError :: InterpreterError -> IO ()
ppError (UnknownError e) = putStrLn $ "UnknownError: " ++ e
ppError (WontCompile es) = putStr . unlines . map errMsg $ es
ppError (NotAllowed e)   = putStrLn $ "NotAllowed: " ++ e
ppError (GhcException e) = putStrLn $ "GhcException: " ++ e  -- TODO: can we actually recover from this?


-- | Given the diagram's source code and options for the cairo
--   backend, build the diagram (in the context of standard imports) and
--   render it as requested
buildDiagram :: String -> Options Cairo R2 -> IO ()
buildDiagram source opts = do
  tmpDir <- getTemporaryDirectory
  (tmp, h) <- openTempFile tmpDir "Diagram.lhs"
  hPutStr h (diagramFileHeader $ takeBaseName tmp)
  hPutStr h source
  hClose h
  compileExample tmp opts
  removeFile tmp

diagramFileHeader :: String -> String
diagramFileHeader modName = unlines $
  [ "> {-# LANGUAGE NoMonomorphismRestriction, DeriveDataTypeable #-}"
  , "> module " ++ modName ++ " where"
  , "> import Diagrams.Prelude"
  , "> import Diagrams.Backend.Cairo"
  , "> import Data.Typeable"
  ]
-}