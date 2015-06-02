{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

module Main where

import           Diagrams.Backend.SVG
import           Diagrams.Builder
import           Diagrams.Prelude       hiding (height, width)
import           Lucid.Svg

import           System.Console.CmdArgs
import           System.Directory       (copyFile, createDirectoryIfMissing)
import qualified System.FilePath        as FP

compileExample :: Build -> IO ()
compileExample (Build{..}) = do
  f <- readFile srcFile

  createDirectoryIfMissing True dir

  let bopts = mkBuildOpts SVG zero (SVGOptions (mkSizeSpec2D width height) Nothing "" )
                & snippets .~ [f]
                & imports  .~ [ "Diagrams.Backend.SVG" ]
                & diaExpr  .~ expr
                & decideRegen .~ (hashedRegenerate (\_ opts -> opts) dir)

  res <- buildDiagram bopts

  case res of
    ParseErr err    -> putStrLn ("Parse error in " ++ srcFile) >> putStrLn err
    InterpErr ierr  -> putStrLn ("Error while compiling " ++ srcFile) >>
                       putStrLn (ppInterpError ierr)
    Skipped hash    -> copyFile (mkFile (hashToHexStr hash)) outFile
    OK hash svg     -> do let cached = mkFile (hashToHexStr hash)
                          renderToFile cached svg
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
