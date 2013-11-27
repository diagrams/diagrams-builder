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
-- Copyright   :  (c) 2013 diagrams-lib team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- Options for dynamic creation of diagrams.
--
-----------------------------------------------------------------------------
module Diagrams.Builder.Opts
    (
      BuildOpts(..), mkBuildOpts, backendOpts, snippets, pragmas, imports, decideRegen, diaExpr, postProcess
    )
    where

import           Control.Lens     (Lens, Lens', generateSignatures, lensRules,
                                   makeLensesWith, (&), (.~))

import           Diagrams.Prelude (Diagram, Options)

-- | Options to control the behavior of @buildDiagram@.  Create one
--   with 'mkBuildOpts' followed by using the provided lenses to
--   override more fields; for example,
--
-- @
--   mkBuildOpts SVG zeroV (Options ...)
--     & imports .~ [\"Foo.Bar\", \"Baz.Quux\"]
--     & diaExpr .~ \"square 6 # fc green\"
-- @
data BuildOpts b v x
  = BuildOpts
    { backendToken :: b
      -- ^ Backend token
    , vectorToken  :: v
      -- ^ Dummy vector argument to fix the vector space type
    , _backendOpts :: Options b v
    , _snippets    :: [String]
    , _pragmas     :: [String]
    , _imports     :: [String]
    , _decideRegen :: String -> IO (x, Maybe (Options b v -> Options b v))
    , _diaExpr     :: String
    , _postProcess :: Diagram b v -> Diagram b v
    }

-- | Create a @BuildOpts@ record with default options:
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
mkBuildOpts :: b -> v -> Options b v -> BuildOpts b v ()
mkBuildOpts b v opts
  = BuildOpts b v opts [] [] [] (const (return ((), Just id))) "circle 1" id

makeLensesWith (lensRules & generateSignatures .~ False) ''BuildOpts

-- | Backend-specific options to use.
backendOpts :: Lens' (BuildOpts b v x) (Options b v)

-- | Source code snippets.  Each should be a syntactically valid
--   Haskell module.  They will be combined intelligently, /i.e./
--   not just pasted together textually but combining pragmas,
--   imports, /etc./ separately.
snippets :: Lens' (BuildOpts b v x) [String]

-- | Extra @LANGUAGE@ pragmas to use (@NoMonomorphismRestriction@
--   is automatically enabled.)
pragmas :: Lens' (BuildOpts b v x) [String]

-- | Additional module imports (note that "Diagrams.Prelude" is
--   automatically imported).
imports :: Lens' (BuildOpts b v x) [String]

-- | A function to decide whether a particular diagram needs to
--   be regenerated.  It will be passed the final assembled
--   source for the diagram (but with the module name set to
--   @Main@ instead of something auto-generated, so that hashing
--   the source will produce consistent results across runs). It
--   can return some information (such as a hash of the source)
--   via the @x@ result, which will be passed through to the
--   result of 'buildDiagram'.  More importantly, it decides
--   whether the diagram should be built: a result of 'Just'
--   means the diagram /should/ be built; 'Nothing' means it
--   should not. In the case that it should be built, it returns
--   a function for updating the rendering options.  This could
--   be used, /e.g./, to request a filename based on a hash of
--   the source.
--
--   Two standard decision functions are provided for
--   convenience: 'alwaysRegenerate' returns no extra information
--   and always decides to regenerate the diagram;
--   'hashedRegenerate' creates a hash of the diagram source and
--   looks for a file with that name in a given directory.
decideRegen :: Lens (BuildOpts b v x) (BuildOpts b v x')
                    (String -> IO (x, Maybe (Options b v -> Options b v)))
                    (String -> IO (x', Maybe (Options b v -> Options b v)))

-- | The diagram expression to interpret.  All the given import sand
--   snippets will be in scope, with the given LANGUAGE pragmas
--   enabled.  The expression may have either of the types @Diagram b
--   v@ or @IO (Diagram b v)@.
diaExpr :: Lens' (BuildOpts b v x) String

-- | A function to apply to the interpreted diagram prior to
--   rendering.  For example, you might wish to apply @pad 1.1
--   . centerXY@.  This is preferred over directly modifying the
--   string expression to be interpreted, since it gives better
--   typechecking, and works no matter whether the expression
--   represents a diagram or an IO action.
postProcess :: Lens' (BuildOpts b v x) (Diagram b v -> Diagram b v)
