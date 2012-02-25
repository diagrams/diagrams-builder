{-# LANGUAGE StandaloneDeriving
           , DeriveDataTypeable
           , ScopedTypeVariables
           , FlexibleContexts
  #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.Builder
-- Copyright   :  (c) 2012 diagrams-lib team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- Tools for dynamically building diagrams, for e.g. creating
-- preprocessors to interpret diagrams code embedded in documents.
--
-----------------------------------------------------------------------------
module Diagrams.Builder where

import Diagrams.Prelude hiding ((<.>), e)

import Language.Haskell.Interpreter hiding (ModuleName)

import System.IO
import System.FilePath
import System.Directory

import Data.List (isPrefixOf, nub)

import Language.Haskell.Exts

import Data.Typeable
deriving instance Typeable Any

------------------------------------------------------------
-- Interpreting diagrams
------------------------------------------------------------

-- | Set up the module to be interpreted, in the context of the
--   necessary imports.
setDiagramImports :: MonadInterpreter m
                  => String      -- ^ Filename of the module containing the diagrams
                  -> [String]    -- ^ Additional necessary
                                 --   imports. @Prelude@,
                                 --   @Diagrams.Prelude@,
                                 --   @Graphics.Rendering.Diagrams.Core@,
                                 --   and @Data.Monoid@ are included
                                 --   by default.
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
ppError (WontCompile  es)  = putStr . unlines . nub . map errMsg $ es
ppError (NotAllowed   err) = putStrLn $ "NotAllowed: "   ++ err
ppError (GhcException err) = putStrLn $ "GhcException: " ++ err

------------------------------------------------------------
-- Manipulating modules
------------------------------------------------------------

-- | Extend some source code into a proper module, by (possibly)
--   giving it a different name and adding LANGUAGE pragmas and
--   imports if necessary.  Only those pragmas and imports which are
--   not already included in the code will be added.
--
--   Returns a string representing the updated module, or an error
--   message if parsing failed.
extendModule :: Maybe String -- ^ Module name to use
             -> [String]     -- ^ @LANGUAGE@ pragmas to add
             -> [String]     -- ^ Imports to add
             -> String       -- ^ Source code
             -> Either String String
extendModule nm langs imps src =
  case parseFileContents src of
    ParseFailed _ err -> Left err
    ParseOk m ->
        Right
      . prettyPrint
      . deleteExports
      . maybe id replaceModuleName nm
      . addPragmas langs
      . addImports imps
      $ m

-- | Create a module with some given code in a temporary file.  The
--   given code may be just a code framgent, or a complete module;
--   pragmas, a module declaration, and imports are added as needed
--   (without overwriting any already present).  Returns either an
--   error string or a path to the generated module.  The caller is
--   responsible for deleting this file when done.
genTempModule :: String      -- ^ Source code
              -> [String]    -- ^ Extra @LANGUAGE@ pragmas to use.
              -> [String]    -- ^ Additional imports.
              -> IO (Either String FilePath)
genTempModule source langs imps = do
  let source' = unLit source
  tmpDir   <- getTemporaryDirectory
  (tmp, h) <- openTempFile tmpDir ("Diagram.hs")
  case extendModule (Just $ takeBaseName tmp) langs imps source' of
    Left  err -> removeFile tmp >> return (Left err)
    Right m   -> do
      hPutStr h m
      hClose h
      return (Right tmp)

-- | Remove all the literate comments and bird tracks from a literate
--   Haskell file.  Has no effect on non-literate source.
unLit :: String -> String
unLit src
  | any ("> " `isPrefixOf`) ls = unlines . map (drop 2) . filter ("> " `isPrefixOf`) $ ls
  | otherwise = src
  where ls = lines src

-- | Dummy value to use when we have to use a @SrcLoc@.
emptyLoc :: SrcLoc
emptyLoc = SrcLoc "" 0 0

-- | Replace the name of a module.
replaceModuleName :: String -> Module -> Module
replaceModuleName m (Module l _ p w e i d) = Module l (ModuleName m) p w e i d

-- | Delete module exports.
deleteExports :: Module -> Module
deleteExports (Module l n p w _ i d) = Module l n p w Nothing i d

-- | Add some @LANGUAGE@ pragmas to a module if necessary.
addPragmas :: [String] -> Module -> Module
addPragmas langs (Module l n p w e i d) = Module l n (f p) w e i d
  where f [] = [LanguagePragma emptyLoc (map Ident langs)]
        f (LanguagePragma loc ps : rest) = LanguagePragma loc (ps ++ map Ident langs) : rest
        f (x : rest) = x : f rest

-- | Add some imports to a module if necessary.
addImports :: [String] -> Module -> Module
addImports imps (Module l n p w e i d) = Module l n p w e (foldr addImport i imps) d
  where addImport imp is
          | any ((==imp) . getModuleName . importModule) is = is
          | otherwise = ImportDecl emptyLoc (ModuleName imp) False False Nothing Nothing Nothing : is

-- | Convert a @ModuleName@ to a @String@.
getModuleName :: ModuleName -> String
getModuleName (ModuleName n) = n

------------------------------------------------------------
-- Build a diagram using a temporary file
------------------------------------------------------------

-- | Build a diagram by writing the given source code to a temporary
--   module and interpreting the given expression.  Can return either
--   a parse error if the source does not parse, an interpreter error,
--   or the final result.
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
             -> IO (Either String (Either InterpreterError (Result b v)))
buildDiagram b v opts source dexp langs imps = do
  mtmp <- genTempModule source ("NoMonomorphismRestriction" : langs)
                               ("Diagrams.Prelude" : imps)
  case mtmp of
    Left err -> return (Left err)
    Right tmp -> do
      compilation <- interpretDiagram b v opts tmp imps dexp
      removeFile tmp
      return (Right compilation)
