{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import           Diagrams.Backend.SVG
import           Diagrams.Builder
import           Diagrams.Prelude hiding (width, height)

import qualified Data.ByteString.Lazy as BS
import           System.Console.CmdArgs
import           System.Directory (createDirectoryIfMissing, copyFile)
import qualified System.FilePath as FP
import           Text.Blaze.Svg.Renderer.Utf8 (renderSvg)

compileExample :: Build -> IO ()
compileExample (Build{..}) = do
  f   <- readFile srcFile

  createDirectoryIfMissing True dir

  res <- buildDiagram
           SVG
           zeroV
           (SVGOptions (mkSizeSpec width height) Nothing)
           [f]
           expr
           []
           [ "Diagrams.Backend.SVG" ]
           (hashedRegenerate (\_ opts -> opts) dir)

  case res of
    ParseErr err    -> putStrLn ("Parse error in " ++ srcFile) >> putStrLn err
    InterpErr ierr  -> putStrLn ("Error while compiling " ++ srcFile) >>
                       putStrLn (ppInterpError ierr)
    Skipped hash    -> copyFile (mkFile hash) outFile
    OK hash svg     -> do let cached = mkFile hash
                          BS.writeFile cached (renderSvg svg)
                          copyFile cached outFile
 where
   mkFile base = dir FP.</> base FP.<.> "svg"

build :: Build
build =
  defaultBuildOpts
  { outFile = "out.svg" &= typFile &= help "Output file (default: \"out.svg\")"
  }
  &= summary "The diagrams-builder-svg program, for dynamically rendering diagrams using the native SVG backend.  Give it a source file and an expression to render (which may refer to things declared in the source file), and it outputs an image, using hashing to avoid rerendering images unnecessarily."
  &= program "diagrams-builder-svg"

main :: IO ()
main = do
  opts <- cmdArgs build
  compileExample opts
