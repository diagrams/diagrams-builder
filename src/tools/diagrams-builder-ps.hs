{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import System.Directory (createDirectoryIfMissing, copyFile)
import qualified System.FilePath as FP

import System.Console.CmdArgs

import Diagrams.Prelude hiding (width, height)
import Diagrams.Backend.Postscript
import Diagrams.Builder

compileExample :: Build -> IO ()
compileExample (Build{..}) = do
  f   <- readFile srcFile

  createDirectoryIfMissing True dir

  res <- buildDiagram
           Postscript
           zeroV
           (PostscriptOptions outFile (mkSizeSpec width height) EPS)
           [f]
           expr
           []
           [ "Diagrams.Backend.Postscript" ]
           (hashedRegenerate
             (\hash opts -> opts { psfileName = mkFile hash })
             dir
           )

  case res of
    ParseErr err    -> putStrLn ("Parse error in " ++ srcFile) >> putStrLn err
    InterpErr ierr  -> putStrLn ("Error while compiling " ++ srcFile) >>
                       putStrLn (ppInterpError ierr)
    Skipped hash    -> copyFile (mkFile hash) outFile
    OK hash act     -> do act >> copyFile (mkFile hash) outFile
 where
   mkFile base = dir FP.</> base FP.<.> "eps"

build :: Build
build =
  defaultBuildOpts
  { outFile = "out.eps" &= typFile &= help "Output file (default: \"out.eps\")"
  }
  &= summary "The diagrams-builder-ps program, for dynamically rendering diagrams using the native postscript backend.  Give it a source file and an expression to render (which may refer to things declared in the source file), and it outputs an image, using hashing to avoid rerendering images unnecessarily."
  &= program "diagrams-builder-ps"

main :: IO ()
main = do
  opts <- cmdArgs build
  compileExample opts