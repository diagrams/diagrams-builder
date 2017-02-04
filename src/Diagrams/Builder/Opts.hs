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
-- Copyright   :  (c) 2013-2015 diagrams-lib team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- Options for dynamic creation of diagrams.
--
-----------------------------------------------------------------------------
module Diagrams.Builder.Opts
  ( -- * Options
    BuildOpts(..)
  , mkBuildOpts
  , backendOpts
  , snippets
  , pragmas
  , imports
  , qimports
  , hashCache
  , diaExpr
  , postProcess

    )
    where

import           Control.Lens     (Lens', generateSignatures, lensRules,
                                   makeLensesWith, (&), (.~))

import           Diagrams.Prelude (Diagram, V, Any)
import           Diagrams.Backend (Options)


-- | Options to control the behavior of @buildDiagram@.  Create one
--   with 'mkBuildOpts' followed by using the provided lenses to
--   override more fields; for example,
--
-- @
--   mkBuildOpts SVG zero (Options ...)
--     & imports .~ [\"Foo.Bar\", \"Baz.Quux\"]
--     & diaExpr .~ \"square 6 # fc green\"
-- @
data BuildOpts b = BuildOpts
  { backendToken :: b
    -- ^ Backend token
  -- , vectorToken  :: v n
  --   -- ^ Dummy vector argument to fix the vector space type
  , _backendOpts :: Options b
  , _snippets    :: [String]
  , _pragmas     :: [String]
  , _imports     :: [String]
  , _qimports    :: [(String, String)]
  , _hashCache   :: Maybe FilePath
  , _diaExpr     :: String
  , _postProcess :: Diagram (V b) -> Diagram (V b)
  }

makeLensesWith (lensRules & generateSignatures .~ False) ''BuildOpts

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
mkBuildOpts :: b -> Options b -> BuildOpts b
mkBuildOpts b opts
  = BuildOpts b opts [] [] [] [] Nothing "circle 1" id

-- | Backend-specific options to use.
backendOpts :: Lens' (BuildOpts b) (Options b)

-- | Source code snippets.  Each should be a syntactically valid
--   Haskell module.  They will be combined intelligently, /i.e./
--   not just pasted together textually but combining pragmas,
--   imports, /etc./ separately.
snippets :: Lens' (BuildOpts b) [String]

-- | Extra @LANGUAGE@ pragmas to use (@NoMonomorphismRestriction@
--   is automatically enabled.)
pragmas :: Lens' (BuildOpts b) [String]

-- | Additional module imports (note that "Diagrams.Prelude" is
--   automatically imported).
imports :: Lens' (BuildOpts b) [String]

-- | Additional qualified module imports (module name, qualified
--   name).
qimports :: Lens' (BuildOpts b) [(String, String)]

-- | A function to decide whether a particular diagram needs to be
--   regenerated.  It will be passed a hash of the final assembled
--   source for the diagram (but with the module name set to @Main@
--   instead of something auto-generated, so that hashing the source
--   will produce consistent results across runs), plus any options,
--   local imports, and other things which could affect the result of
--   rendering. It can return some information (such as a hash of the
--   source) via the @x@ result, which will be passed through to the
--   result of 'buildDiagram'.  More importantly, it decides whether
--   the diagram should be built: a result of 'Just' means the diagram
--   /should/ be built; 'Nothing' means it should not. In the case
--   that it should be built, it returns a function for updating the
--   rendering options.  This could be used, /e.g./, to request a
--   filename based on a hash of the source.
--
--   Two standard decision functions are provided for
--   convenience: 'alwaysRegenerate' returns no extra information
--   and always decides to regenerate the diagram;
--   'hashedRegenerate' creates a hash of the diagram source and
--   looks for a file with that name in a given directory.
-- decideRegen :: Lens' (BuildOpts b) (Hash -> IO (Maybe (Options b -> Options b)))

-- | Only rebuild the diagram if the hash has changed.
hashCache :: Lens' (BuildOpts b) (Maybe FilePath)

-- | The diagram expression to interpret.  All the given import sand
--   snippets will be in scope, with the given LANGUAGE pragmas
--   enabled.  The expression may have either of the types @Diagram b@
--   or @IO (Diagram b)@.
diaExpr :: Lens' (BuildOpts b) String

-- | A function to apply to the interpreted diagram prior to
--   rendering.  For example, you might wish to apply @pad 1.1
--   . centerXY@.  This is preferred over directly modifying the
--   string expression to be interpreted, since it gives better
--   typechecking, and works no matter whether the expression
--   represents a diagram or an IO action.
postProcess :: Lens' (BuildOpts b) (Diagram (V b) -> Diagram (V b))

