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
    ( -- * Options

      Hash
    , BuildOpts(..), mkBuildOpts, backendOpts, snippets, pragmas, imports, qimports, decideRegen, diaExpr, postProcess

      -- * Rebuilding

    , alwaysRegenerate, hashedRegenerate, hashToHexStr
    )
    where

import           Control.Lens     (Lens', generateSignatures, lensRules,
                                   makeLensesWith, (&), (.~))
import           System.Directory (getDirectoryContents)
import           System.FilePath  (takeBaseName)
import           Text.Printf

import           Diagrams.Prelude (Any, Options, QDiagram)

-- | Synonym for more perspicuous types.
--
--   We use @Int@ values for hashes because that's what the @Hashable@
--   package uses.  Assuming diagram hashes are uniformly distributed,
--   on a 64-bit system one needs to build on the order of billions of
--   diagrams before the probability of a hash collision exceeds 1/2,
--   and for anything up to tens of millions of diagrams the
--   probability of a collision is under 0.1%.  On 32-bit systems
--   those become tens of thousands and thousands, respectively.
type Hash = Int

-- | Options to control the behavior of @buildDiagram@.  Create one
--   with 'mkBuildOpts' followed by using the provided lenses to
--   override more fields; for example,
--
-- @
--   mkBuildOpts SVG zeroV (Options ...)
--     & imports .~ [\"Foo.Bar\", \"Baz.Quux\"]
--     & diaExpr .~ \"square 6 # fc green\"
-- @
data BuildOpts b v n
  = BuildOpts
    { backendToken :: b
      -- ^ Backend token
    , vectorToken  :: v n
      -- ^ Dummy vector argument to fix the vector space type
    , _backendOpts :: Options b v n
    , _snippets    :: [String]
    , _pragmas     :: [String]
    , _imports     :: [String]
    , _qimports    :: [(String, String)]
    , _decideRegen :: Hash -> IO (Maybe (Options b v n -> Options b v n))
    , _diaExpr     :: String
    , _postProcess :: QDiagram b v n Any -> QDiagram b v n Any
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
mkBuildOpts :: b -> v n -> Options b v n -> BuildOpts b v n
mkBuildOpts b v opts
  = BuildOpts b v opts [] [] [] [] alwaysRegenerate "circle 1" id

-- | Backend-specific options to use.
backendOpts :: Lens' (BuildOpts b v n) (Options b v n)

-- | Source code snippets.  Each should be a syntactically valid
--   Haskell module.  They will be combined intelligently, /i.e./
--   not just pasted together textually but combining pragmas,
--   imports, /etc./ separately.
snippets :: Lens' (BuildOpts b v n) [String]

-- | Extra @LANGUAGE@ pragmas to use (@NoMonomorphismRestriction@
--   is automatically enabled.)
pragmas :: Lens' (BuildOpts b v n) [String]

-- | Additional module imports (note that "Diagrams.Prelude" is
--   automatically imported).
imports :: Lens' (BuildOpts b v n) [String]

-- | Additional qualified module imports (module name, qualified
--   name).
qimports :: Lens' (BuildOpts b v n) [(String, String)]

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
decideRegen :: Lens' (BuildOpts b v n) (Hash -> IO (Maybe (Options b v n -> Options b v n)))

-- | The diagram expression to interpret.  All the given import sand
--   snippets will be in scope, with the given LANGUAGE pragmas
--   enabled.  The expression may have either of the types @Diagram b@
--   or @IO (Diagram b)@.
diaExpr :: Lens' (BuildOpts b v n) String

-- | A function to apply to the interpreted diagram prior to
--   rendering.  For example, you might wish to apply @pad 1.1
--   . centerXY@.  This is preferred over directly modifying the
--   string expression to be interpreted, since it gives better
--   typechecking, and works no matter whether the expression
--   represents a diagram or an IO action.
postProcess :: Lens' (BuildOpts b v n) (QDiagram b v n Any -> QDiagram b v n Any)

-- | Convenience function suitable to be given as the final argument
--   to 'buildDiagram'.  It implements the simple policy of always
--   rebuilding every diagram.
alwaysRegenerate :: Hash -> IO (Maybe (a -> a))
alwaysRegenerate _ = return (Just id)

-- | Convenience function suitable to be given as the final argument
--   to 'buildDiagram'.  It works by converting the hash value to a
--   zero-padded hexadecimal string and looking in the specified
--   directory for any file whose base name is equal to the hash.  If
--   there is such a file, it specifies that the diagram should not be
--   rebuilt.  Otherwise, it specifies that the diagram should be
--   rebuilt, and uses the provided function to update the rendering
--   options based on the generated hash string.  (Most likely, one
--   would want to set the requested output file to the hash followed
--   by some extension.)
hashedRegenerate
  :: (String -> a -> a)
     -- ^ A function for computing an update to rendering options,
     --   given a new base filename computed from a hash of the
     --   diagram source.

  -> FilePath
     -- ^ The directory in which to look for generated files

  -> Hash
     -- ^ The hash

  -> IO (Maybe (a -> a))

hashedRegenerate upd d hash = do
  let fileBase = hashToHexStr hash
  files <- getDirectoryContents d
  case any ((fileBase==) . takeBaseName) files of
    True  -> return Nothing
    False -> return $ Just (upd fileBase)

hashToHexStr :: Hash -> String
hashToHexStr n = printf "%016x" n'
  where
    n' :: Integer
    n' = fromIntegral n - fromIntegral (minBound :: Int)
