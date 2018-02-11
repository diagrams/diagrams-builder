{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.Builder.CmdLine
-- Copyright   :  (c) 2012 diagrams-lib team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- Tools for creating standalone command-line diagram builder utilities.
--
-----------------------------------------------------------------------------
module Diagrams.Builder.CmdLine where
    -- ( Build(..)
    -- , defaultBuildOpts
    -- )
    -- where

import Options.Applicative
import Diagrams.Prelude
import Diagrams.Backend
import Diagrams.Builder
import Diagrams.Builder.Opts

-- | Record of command-line options.
data Build = Build
  { buildSize :: SizeSpec V2 Int
  , cacheDir  :: FilePath
  , srcFile   :: FilePath
  , outFile   :: FilePath
  , bExpr :: String
  }

buildParser :: Parser Build
buildParser = Build <$> sizeParser <*> cacheParser <*> srcParser <*> outputParser <*> exprParser
  where
    cacheParser = strOption $ mconcat
      [ long "cache", short 'c', metavar "FILEPATH"
      , help "Folder to cache diagrams", value ".diagrams_cache" ]
    srcParser = strArgument $ mconcat
      [ metavar "FILEPATH", help "Source file" ]
    exprParser = strOption $ mconcat
      [ long "expr", short 'e', metavar "STRING"
      , help "Expression to evaluate for the value of the diagram"
      , value "diagram"]

buildSnippet :: Build -> IO Snippet
buildSnippet Build {..} = do
  src <- readFile srcFile
  return $ emptySnippet &~ do
    snippets .= [src]
    imports  .= ["Prelude", "Diagrams.Prelude", "Diagrams.Backend", "Geometry"]
    pragmas  .= ["NoMonomorphismRestriction", "FlexibleContexts"]

simple :: Build -> BackendInfo -> IO ()
simple build info = do
  snippet <- buildSnippet build
  let buildOpts = DiaBuildOpts
        { _cachePath = Just (cacheDir build)
        , _buildInfo = info
        , _buildTarget = outFile build
        , _diaOutputSize = buildSize build
        , _diaBuildExpr = bExpr build
        }

  r <- saveDiaBuilder snippet buildOpts
  case r of
    ParseError err    -> putStrLn "Parse error:" >> putStrLn err
    InterpError ierr  -> putStrLn "Compile error:" >>
                         putStrLn (ppInterpError ierr)
    Skipped hash      -> return () -- copyFile (mkFile (hashToHexStr hash)) outFile
    OK hash svg       -> return ()

