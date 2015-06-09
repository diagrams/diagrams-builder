{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards    #-}

module Main where

import qualified Data.ByteString.Builder     as BSB
import qualified Data.ByteString.Lazy        as BSL
import           Diagrams.Backend.PGF
import           Diagrams.Backend.PGF.Render (Options (..))
import           Diagrams.Builder
import           Diagrams.Prelude            hiding (height, width)

import           System.Directory            (copyFile,
                                              createDirectoryIfMissing)
import qualified System.FilePath             as FP

import           System.Console.CmdArgs      hiding (def)

compileExample :: Build -> IO ()
compileExample (Build{..}) = do
  let ext = FP.takeExtension outFile
      standalone = case ext of
                     ".pgf" -> False
                     ".tex" -> True
  f   <- readFile srcFile

  createDirectoryIfMissing True dir

  let w = fmap realToFrac width :: Maybe Double
      h = fmap realToFrac height :: Maybe Double
      bopts = mkBuildOpts PGF (zero :: V2 Double) (PGFOptions def (mkSizeSpec2D w h) False standalone)
                & snippets .~ [f]
                & imports  .~ [ "Diagrams.Backend.PGF" ]
                & diaExpr  .~ expr
                & decideRegen .~ (hashedRegenerate (\_ opts -> opts) dir)

  res <- buildDiagram bopts
  case res of
    ParseErr err    -> putStrLn ("Parse error in " ++ srcFile) >> putStrLn err
    InterpErr ierr  -> putStrLn ("Error while compiling " ++ srcFile) >>
                       putStrLn (ppInterpError ierr)
    Skipped hash    -> copyFile (mkFile (hashToHexStr hash) ext) outFile
    OK hash pgf -> let cached = mkFile (hashToHexStr hash) ext
                   in do BSL.writeFile cached (BSB.toLazyByteString pgf)
                         copyFile cached outFile
 where
  mkFile base ext = dir FP.</> base FP.<.> ext

build :: Build
build =
  defaultBuildOpts
  { outFile  = "out.pgf" &= typFile &= help "Output file (default: \"out.pgf\")" }
  &= summary "The diagrams-builder-PGF program, for dynamically rendering diagrams using the PGF backend.  Give it a source file and an expression to render (which may refer to things declared in the source file), and it outputs an image, using hashing to avoid rerendering images unnecessarily. Use a .pgf extension to get PGF code, suitable for inputting directly into a LaTeX file.  Use a .tex extensions to get a standalone TeX file.\n\nIf you have installed the diagrams-lib and diagrams-pgf packages in a sandbox, set the environment variable DIAGRAMS_SANDBOX to that directory so the builder can compile your diagram. For example, \"export DIAGRAMS_SANDBOX=~/diagrams/.cabal-sandbox\". If you have package databases for multiple versions of GHC in that sandbox, you will need to set the full path to the package database (it ends in \".conf.d\")."
  &= program "diagrams-builder-pgf"

main :: IO ()
main = do
  opts <- cmdArgs build
  compileExample opts
