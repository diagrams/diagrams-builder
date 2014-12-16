{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards    #-}

module Main where
import qualified Codec.Picture as J
import           Diagrams.Backend.Rasterific
import           Diagrams.Prelude             hiding (height, width)

import           Diagrams.Builder

import           System.Directory             (copyFile,
                                               createDirectoryIfMissing)
import qualified System.FilePath              as FP

import           System.Console.CmdArgs

compileExample :: Build -> IO ()
compileExample (Build{..}) = do
  let ext = FP.takeExtension outFile
      save = case ext of
               ".png" -> J.savePngImage
               ".jpg" -> J.saveJpgImage 100
               ".bmp" -> J.saveBmpImage
               ".gif" -> (either error id .) . J.saveGifImage
               ".tiff" -> J.saveTiffImage
               _ -> J.savePngImage
  f   <- readFile srcFile

  createDirectoryIfMissing True dir

  let w = fmap realToFrac width
      h = fmap realToFrac height
      bopts = mkBuildOpts Rasterific zero (RasterificOptions (mkSizeSpec2D w h))
                & snippets .~ [f]
                & imports  .~ [ "Diagrams.Backend.Rasterific" ]
                & diaExpr  .~ expr
                & decideRegen .~ (hashedRegenerate (\_ opts -> opts) dir)

  res <- buildDiagram bopts
  case res of
    ParseErr err    -> putStrLn ("Parse error in " ++ srcFile) >> putStrLn err
    InterpErr ierr  -> putStrLn ("Error while compiling " ++ srcFile) >>
                       putStrLn (ppInterpError ierr)
    Skipped hash    -> copyFile (mkFile (hashToHexStr hash) ext) outFile
    OK hash img -> let cached = mkFile (hashToHexStr hash) ext
                   in do save cached (J.ImageRGBA8 img)
                         copyFile cached outFile
 where
  mkFile base ext = dir FP.</> base FP.<.> ext

build :: Build
build =
  defaultBuildOpts
  { outFile  = "out.png" &= typFile &= help "Output file (default: \"out.png\")" }
  &= summary "The diagrams-builder-rasterific program, for dynamically rendering diagrams using the rasterific backend.  Give it a source file and an expression to render (which may refer to things declared in the source file), and it outputs an image, using hashing to avoid rerendering images unnecessarily.\n\nIf you have installed the diagrams-lib and diagrams-rasterific packages in a sandbox, set the environment variable DIAGRAMS_SANDBOX to that directory so the builder can compile your diagram. For example, \"export DIAGRAMS_SANDBOX=~/diagrams/.cabal-sandbox\". If you have package databases for multiple versions of GHC in that sandbox, you will need to set the full path to the package database (it ends in \".conf.d\")."
  &= program "diagrams-builder-rasterific"

main :: IO ()
main = do
  opts <- cmdArgs build
  compileExample opts
