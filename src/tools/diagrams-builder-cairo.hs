{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards    #-}

module Main where

import           Diagrams.Backend.Cairo
import           Diagrams.Backend.Cairo.Internal
import           Diagrams.Prelude                hiding (height, width)

import           Diagrams.Builder

import           System.Directory                (copyFile,
                                                  createDirectoryIfMissing)
import qualified System.FilePath                 as FP

import           System.Console.CmdArgs

compileExample :: Build -> IO ()
compileExample (Build{..}) = do
  let ext = FP.takeExtension outFile
      fmt = case ext of
              ".png" -> PNG
              ".svg" -> SVG
              ".ps"  -> PS
              ".pdf" -> PDF
              _      -> PNG

  f   <- readFile srcFile

  createDirectoryIfMissing True dir

  let bopts = mkBuildOpts Cairo zero (CairoOptions outFile (mkSizeSpec width height) fmt False)
                & snippets .~ [f]
                & imports  .~ [ "Diagrams.Backend.Cairo" ]
                & diaExpr  .~ expr
                & decideRegen
                  .~ (hashedRegenerate
                       (\base opts -> opts & cairoFileName .~ mkFile base ext)
                       dir
                     )

  res <- buildDiagram bopts
  case res of
    ParseErr err    -> putStrLn ("Parse error in " ++ srcFile) >> putStrLn err
    InterpErr ierr  -> putStrLn ("Error while compiling " ++ srcFile) >>
                       putStrLn (ppInterpError ierr)
    Skipped hash    -> copyFile (mkFile (hashToHexStr hash) ext) outFile
    OK hash (act,_) -> act >> copyFile (mkFile (hashToHexStr hash) ext) outFile
 where
  mkFile base ext = dir FP.</> base FP.<.> ext

build :: Build
build =
  defaultBuildOpts
  { outFile  = "out.png" &= typFile &= help "Output file (default: \"out.png\")" }
  &= summary "The diagrams-builder-cairo program, for dynamically rendering diagrams using the cairo backend.  Give it a source file and an expression to render (which may refer to things declared in the source file), and it outputs an image, using hashing to avoid rerendering images unnecessarily."
  &= program "diagrams-builder-cairo"

main :: IO ()
main = do
  opts <- cmdArgs build
  compileExample opts
