-- Module SrcLoc ModuleName [ModulePragma] (Maybe WarningText) (Maybe [ExportSpec]) [ImportDecl] [Decl]
--
-- Module :: Maybe ModuleHead -> [ModulePragma] -> [ImportDecl] -> [Decl] -> Module
-- ModuleHead :: ModuleName -> Maybe WarningText -> Maybe ExportSpecList -> ModuleHead

-- so (Module sl *mn ps *w *exp imp decl)
--
-- -> Module (Just (ModuleHead mn w exp)) ps imp decl

{-# LANGUAGE TupleSections #-}

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

import           Control.Arrow                (second)
import           Control.Lens                 ((^.))
import           Data.Function                (on)
import           Data.Functor                 ((<$>))
import           Data.List                    (foldl', groupBy, isPrefixOf, nub,
                                               sortBy)
import           Data.Maybe                   (isJust)
import           Data.Ord                     (comparing)

import           Language.Haskell.Exts.Simple

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
             -> BuildOpts b
             -> Either String Module
createModule nm opts = do
  ms <- mapM doModuleParse (opts ^. snippets)
  return
    . deleteExports
    . maybe id replaceModuleName nm
    . addPragmas (opts ^. pragmas)
    . addImports (map (,Nothing) (opts ^. imports) ++ map (second Just) (opts ^. qimports))
    . foldl' combineModules emptyModule
    $ ms

emptyModule :: Module
emptyModule = Module (Just (ModuleHead (ModuleName "Main") Nothing Nothing)) [] [] []

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
replaceModuleName m (Module Nothing p i d)
  = Module (Just (ModuleHead (ModuleName m) Nothing Nothing)) p i d
replaceModuleName m (Module (Just (ModuleHead _ w e)) p i d)
  = Module (Just (ModuleHead (ModuleName m) w e)) p i d

-- | Delete module exports.
deleteExports :: Module -> Module
deleteExports m@(Module Nothing _ _ _) = m
deleteExports (Module (Just (ModuleHead n w _)) p i d)
  = Module (Just (ModuleHead n w Nothing)) p i d

-- | Add some @LANGUAGE@ pragmas to a module if necessary.
addPragmas :: [String] -> Module -> Module
addPragmas langs (Module h p i d) = Module h (f p) i d
  where f [] = [LanguagePragma (map Ident langs)]
        f (LanguagePragma ps : rest) = LanguagePragma (ps ++ map Ident langs) : rest
        f (x : rest) = x : f rest

-- | Add some imports to a module if necessary.
addImports :: [(String, Maybe String)] -> Module -> Module
addImports imps (Module h p i d) = Module h p (foldr addImport i imps) d
  where addImport (imp, mq) is
          | any (sameImport imp mq) is = is
          | otherwise = ImportDecl (ModuleName imp) (isJust mq) False False
                                   Nothing (ModuleName <$> mq) Nothing : is
        sameImport imp mq imp' =
             ((==imp) . getModuleName . importModule) imp'
          && (isJust mq == importQualified imp')
          && ((ModuleName <$> mq) == importAs imp')

-- | Combine two modules into one, with a left bias in the case of
--   things that can't be sensibly combined (/e.g./ the module name).
--   Note that combining multiple imports of the same module with
--   different import specifications (qualification, hiding, explicit
--   import) is unlikely to work sensibly.
combineModules :: Module -> Module -> Module
combineModules (Module h ps1 i1 d1)
               (Module _ ps2 i2 d2) =
    Module h combinedPragmas combinedImports (d1 ++ d2)
  where
    combinedPragmas = combinedLangPragmas ++ otherPragmas ps1 ++ otherPragmas ps2
    combinedImports = map head
                    . groupBy ((==) `on` importModule)
                    . sortBy (comparing importModule)
                    $ i1 ++ i2

    combinedLangPragmas
      = [LanguagePragma (nub (getLangPragmas ps1 ++ getLangPragmas ps2))]

    getLangPragmas = concatMap getLangPragma
    getLangPragma (LanguagePragma ns) = ns
    getLangPragma _                   = []

    otherPragmas = filter (not . isLangPragma)
    isLangPragma (LanguagePragma {}) = True
    isLangPragma _                   = False

-- | Convert a @ModuleName@ to a @String@.
getModuleName :: ModuleName -> String
getModuleName (ModuleName n) = n

