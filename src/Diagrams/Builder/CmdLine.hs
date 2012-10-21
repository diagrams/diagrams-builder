{-# LANGUAGE DeriveDataTypeable #-}

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
module Diagrams.Builder.CmdLine
    ( Build(..)
    , defaultBuildOpts
    )
    where

import System.Console.CmdArgs

-- | Record of command-line options.
data Build = Build { width   :: Maybe Double
                   , height  :: Maybe Double
                   , srcFile :: String
                   , expr    :: String
                   , outFile :: String
                   , dir     :: String
                   }
  deriving (Typeable, Data)

-- | Default command-line options record.
defaultBuildOpts :: Build
defaultBuildOpts =
  Build
  { width    = def &= typ "INT"
  , height   = def &= typ "INT"
  , srcFile  = "" &= argPos 0 &= typFile
  , expr     = "dia"
               &= typ "EXPRESSION"
               &= help "Expression to render (default: \"dia\")"
  , outFile  = def &= typFile &= help "Output file"
  , dir      = "diagrams"
               &= typDir
               &= help "Directory in which to store rendered diagrams by hash (default: \"diagrams\")"
  }
