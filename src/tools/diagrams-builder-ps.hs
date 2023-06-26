{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE CPP                #-}

module Main where

import qualified Data.ByteString.Builder     as B
import           System.Directory            (copyFile,
                                              createDirectoryIfMissing)
import qualified System.FilePath             as FP
import qualified System.IO                   as IO

import           System.Console.CmdArgs

import           Diagrams.Backend.Postscript
import           Diagrams.Builder
import           Diagrams.Prelude            hiding (height, width)

compileExample :: Build -> IO ()
compileExample (Build{..}) = do
  f   <- readFile srcFile

  createDirectoryIfMissing True dir

  let bopts = mkBuildOpts Postscript zero (PostscriptOptions outFile (mkSizeSpec2D width height) EPS)
                & snippets .~ [f]
                & imports  .~ [ "Diagrams.Backend.Postscript" ]
                & diaExpr  .~ expr
                & decideRegen .~
                    (hashedRegenerate
                      (\hash opts -> opts & psfileName .~ mkFile hash )
                      dir
                    )


  res <- buildDiagram bopts

  case res of
    ParseErr err    -> putStrLn ("Parse error in " ++ srcFile) >> putStrLn err
    InterpErr ierr  -> putStrLn ("Error while compiling " ++ srcFile) >>
                       putStrLn (ppInterpError ierr)
    Skipped hash    -> copyFile (mkFile (hashToHexStr hash)) outFile
    OK hash builder -> do
      let cached = mkFile (hashToHexStr hash)
      writeBuilder cached builder
      copyFile cached outFile
 where
   mkFile base = dir FP.</> base FP.<.> "eps"

writeBuilder :: FilePath -> B.Builder -> IO ()
#if MIN_VERSION_bytestring(0,11,2)
writeBuilder fp b = B.writeFile fp b
#else
writeBuilder fp b = IO.withBinaryFile fp IO.WriteMode (`B.hPutBuilder` b)
#endif
{-# INLINE writeBuilder #-}

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
