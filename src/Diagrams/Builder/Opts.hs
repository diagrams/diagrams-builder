{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# OPTIONS_GHC -fno-warn-orphans  #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.Builder.Opts
-- Copyright   :  (c) 2013-2018 diagrams team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- Options for dynamic creation of diagrams.
--
-----------------------------------------------------------------------------
module Diagrams.Builder.Opts
  ( -- * Snippets
    Snippets(..)
  , emptySnippet
  , snippets
  , pragmas
  , imports
  , qimports
  ) where

import Control.Lens

-- | A bit of code to run with associated imports and pragmas.
data Snippets = Snippets
  { _snippets :: [String]
  , _pragmas  :: [String]
  , _imports  :: [String]
  , _qimports :: [(String, String)]
  } deriving Show

makeLensesWith (lensRules & generateSignatures .~ False) ''Snippets

-- | An empty @Snippets@ record with default options:
--
--   * no snippets
--
--   * no pragmas
--
--   * no imports
--
--   * always regenerate
--
--   * the diagram expression @circle 1@
--
--   * no postprocessing
emptySnippet :: Snippets
emptySnippet = Snippets
  { _snippets    = []
  , _pragmas     = []
  , _imports     = []
  , _qimports    = []
  }

-- | Source code snippets.  Each should be a syntactically valid
--   Haskell module.  They will be combined intelligently, /i.e./
--   not just pasted together textually but combining pragmas,
--   imports, /etc./ separately.
snippets :: Lens' Snippets [String]

-- | Extra @LANGUAGE@ pragmas to use (@NoMonomorphismRestriction@
--   is automatically enabled.)
pragmas :: Lens' Snippets [String]

-- | Additional module imports (note that "Diagrams.Prelude" is
--   automatically imported).
imports :: Lens' Snippets [String]

-- | Additional qualified module imports (module name, qualified
--   name).
qimports :: Lens' Snippets [(String, String)]
