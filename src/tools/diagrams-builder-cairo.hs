{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Diagrams.Prelude hiding (width, height)
import Diagrams.Backend.Cairo
import Diagrams.Backend.Cairo.Internal -- due to GHC export bug in 7.4

import Diagrams.Builder

import System.Directory (copyFile)
import qualified System.FilePath as FP

import System.Console.CmdArgs

compileExample :: Build -> IO ()
compileExample (Build{..}) = do
  let fmt = case FP.takeExtension outFile of
              ".png" -> PNG
              ".svg" -> SVG
              ".ps"  -> PS
              ".pdf" -> PDF
              _      -> PNG

  f   <- readFile srcFile

  res <- buildDiagram
           Cairo
           zeroV
           (CairoOptions outFile (mkSizeSpec width height) fmt)
           [f]
           expr
           []
           [ "Diagrams.Backend.Cairo" ]
           (hashedRegenerate
             (\hash opts -> opts { cairoFileName = mkFile hash })
             dir
           )
  case res of
    ParseErr err    -> putStrLn ("Parse error in " ++ srcFile) >> putStrLn err
    InterpErr ierr  -> putStrLn ("Error while compiling " ++ srcFile) >>
                       putStrLn (ppInterpError ierr)
    Skipped hash    -> copyFile (mkFile hash) outFile
    OK hash (act,_) -> act >> copyFile (mkFile hash) outFile
 where
  mkFile base = dir FP.</> base FP.<.> "png"

data Build = Build { width   :: Maybe Double
                   , height  :: Maybe Double
                   , srcFile :: String
                   , expr    :: String
                   , outFile :: String
                   , dir     :: String
                   }
  deriving (Typeable, Data)

build :: Build
build = Build { width    = def &= typ "INT"
              , height   = def &= typ "INT"
              , srcFile  = "" &= argPos 0 &= typFile
              , expr     = "dia"
                &= typ "EXPRESSION"
                &= help "Expression to render (default: \"dia\")"
              , outFile  = "out.png" &= typFile &= help "Output file (default: \"out.png\")"
              , dir      = "diagrams"
                &= typDir
                &= help "Directory in which to store rendered diagrams by hash (default: \"diagrams\")"
              }
        &= summary "The diagrams-builder-cairo program, for dynamically rendering diagrams using the cairo backend.  Give it a source file and an expression to render (which may refer to things declared in the source file), and it outputs an image, using hashing to avoid rerendering images unnecessarily."
        &= program "diagrams-builder-cairo"

main :: IO ()
main = do
  opts <- cmdArgs build
  compileExample opts