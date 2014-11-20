-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.Builder.Modules
-- Copyright   :  (c) 2012 diagrams-lib team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- Tools used by diagrams-builder for manipulating Haskell modules.
--
-----------------------------------------------------------------------------

module Diagrams.Builder.Modules where

import           Control.Lens                 ((^.))
import           Data.Function                (on)
import           Data.List                    (foldl', groupBy, isPrefixOf, nub,
                                               sortBy)
import           Data.Ord                     (comparing)

import           Language.Haskell.Exts
import           Language.Haskell.Exts.SrcLoc (noLoc)

import           Diagrams.Builder.Opts

------------------------------------------------------------
-- Manipulating modules
------------------------------------------------------------

-- | Extend some snippets of source code into a proper module, by
--   combining them intelligently (preserving imports, pragmas, /etc./),
--   (possibly) giving it a different name, and adding @LANGUAGE@ pragmas
--   and imports if necessary.  Only those pragmas and imports which
--   are not already included in the code will be added.
--
--   Returns the updated module, or an error message if parsing
--   failed.
createModule :: Maybe String -- ^ Module name to use
             -> BuildOpts b v
             -> Either String Module
createModule nm opts = do
  ms <- mapM doModuleParse (opts ^. snippets)
  return
    . deleteExports
    . maybe id replaceModuleName nm
    . addPragmas (opts ^. pragmas)
    . addImports (opts ^. imports)
    . foldl' combineModules emptyModule
    $ ms

emptyModule :: Module
emptyModule = Module noLoc (ModuleName "Main") [] Nothing Nothing [] []

-- | Run the haskell-src-exts parser on a @String@ representing some
--   Haskell code, producing a @Module@ or an error message.
doModuleParse :: String -> Either String Module
doModuleParse src =
  case parseFileContentsWithMode parseMode src of
    ParseFailed sloc err -> Left (prettyPrint sloc ++ ": " ++ err)
    ParseOk m         -> return m
  where
    parseMode
      = defaultParseMode
        { baseLanguage = Haskell2010
        , fixities     = Nothing
        }

-- | Remove all the literate comments and bird tracks from a literate
--   Haskell file.  Has no effect on non-literate source.
unLit :: String -> String
unLit src
  | any ("> " `isPrefixOf`) ls = unlines . map (drop 2) . filter ("> " `isPrefixOf`) $ ls
  | otherwise = src
  where ls = lines src

-- | Replace the name of a module.
replaceModuleName :: String -> Module -> Module
replaceModuleName m (Module l _ p w e i d) = Module l (ModuleName m) p w e i d

-- | Delete module exports.
deleteExports :: Module -> Module
deleteExports (Module l n p w _ i d) = Module l n p w Nothing i d

-- | Add some @LANGUAGE@ pragmas to a module if necessary.
addPragmas :: [String] -> Module -> Module
addPragmas langs (Module l n p w e i d) = Module l n (f p) w e i d
  where f [] = [LanguagePragma noLoc (map Ident langs)]
        f (LanguagePragma lpLoc ps : rest) = LanguagePragma lpLoc (ps ++ map Ident langs) : rest
        f (x : rest) = x : f rest

-- | Add some imports to a module if necessary.
addImports :: [String] -> Module -> Module
addImports imps (Module l n p w e i d) = Module l n p w e (foldr addImport i imps) d
  where addImport imp is
          | any ((==imp) . getModuleName . importModule) is = is
          | otherwise = ImportDecl noLoc (ModuleName imp) False False False
                        Nothing Nothing Nothing : is

-- | Combine two modules into one, with a left bias in the case of
--   things that can't be sensibly combined (/e.g./ the module name).
--   Note that combining multiple imports of the same module with
--   different import specifications (qualification, hiding, explicit
--   import) is unlikely to work sensibly.
combineModules :: Module -> Module -> Module
combineModules (Module l1 n1 ps1 w1 e1 i1 d1)
               (Module _  _  ps2 _  _  i2 d2) =
    Module l1 n1 combinedPragmas w1 e1 combinedImports (d1 ++ d2)
  where
    combinedPragmas = combinedLangPragmas ++ otherPragmas ps1 ++ otherPragmas ps2
    combinedImports = map head
                    . groupBy ((==) `on` importModule)
                    . sortBy (comparing importModule)
                    $ i1 ++ i2

    combinedLangPragmas
      = [LanguagePragma noLoc (nub (getLangPragmas ps1 ++ getLangPragmas ps2))]

    getLangPragmas = concatMap getLangPragma
    getLangPragma (LanguagePragma _ ns) = ns
    getLangPragma _                     = []

    otherPragmas = filter (not . isLangPragma)
    isLangPragma (LanguagePragma {}) = True
    isLangPragma _                   = False

-- | Convert a @ModuleName@ to a @String@.
getModuleName :: ModuleName -> String
getModuleName (ModuleName n) = n
