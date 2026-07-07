{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards    #-}

import           Data.Char          (toLower)
import           Data.List          (find, intercalate)
import           System.Exit        (die)
import           System.FilePath    (takeExtension)

import           Diagrams.Backend
import           Diagrams.Builder
import           Diagrams.Builder.Opts
import           Diagrams.Prelude   hiding (simple, option, value)
import           Options.Applicative

-- | Record of command-line options.
data Build = Build
  { buildBackend :: Maybe String
  , buildSize    :: SizeSpec V2 Int
  , cacheDir     :: FilePath
  , srcFile      :: FilePath
  , outFile      :: FilePath
  , bExpr        :: String
  }

buildParser :: Parser Build
buildParser = Build
  <$> backendParser
  <*> buildSizeParser
  <*> cacheParser
  <*> srcParser
  <*> outputParser
  <*> exprParser
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

-- | Like 'sizeParser' but accepts fractional values (e.g. the TeX unit
--   sizes passed by diagrams-latex.sty), rounding to the nearest pixel.
buildSizeParser :: Parser (SizeSpec V2 Int)
buildSizeParser = mk <$> o w <*> o h where
  mk mw mh = mkSizeSpec2D (round' <$> mw) (round' <$> mh)
  round' = round :: Double -> Int
  o = optional . option auto
  w = mconcat [ long "width", short 'w', metavar "NUM"
              , help "Desired width of the output image" ]
  h = mconcat [ long "height", short 'h', metavar "NUM"
              , help "Desired height of the output image" ]

-- | Backends the builder knows how to generate code for.
availableBackends :: [BackendInfo]
availableBackends = [pgfInfo, svgInfo, rasterificInfo, cairoInfo]

-- | Choose a backend from the @--backend@ flag, falling back to the
--   output file's extension and finally to pgf.
chooseBackendInfo :: Build -> Either String BackendInfo
chooseBackendInfo Build {..} =
  case buildBackend of
    Just nm -> case findBy backendNames nm of
      Just i  -> Right i
      Nothing -> Left $ "unknown backend " ++ show nm
                     ++ "; available backends: " ++ names
    Nothing -> case dropWhile (== '.') (takeExtension outFile) of
      ""  -> Right pgfInfo
      ext -> case findBy backendExtensions ext of
        Just i  -> Right i
        Nothing -> Right pgfInfo
  where
    findBy f s = find (elem (map toLower s) . f) availableBackends
    names = intercalate ", " (concatMap backendNames availableBackends)

-- | Make a snippet from the command line arguments.
buildSnippet :: Build -> IO Snippets
buildSnippet Build {..} = do
  src <- readFile srcFile
  return $ emptySnippet &~ do
    snippets .= [src]
    imports  .= ["Prelude", "Diagrams.Prelude", "Diagrams.Backend", "Geometry"]
    pragmas  .= ["GADTs", "NoMonomorphismRestriction", "FlexibleContexts"]

-- | A simple builder given some backend info.
simple :: Build -> BackendInfo -> IO ()
simple build info = do
  snippet <- buildSnippet build
  let builder = DiagramBuilder
        { _diaInfo    = info
        , _diaExpr    = bExpr build
        , _diaOutSize = buildSize build
        }

  case diaSnippet snippet builder of
    Left err -> putStrLn err
    Right i  -> do
      x <- cacheRun (cacheDir build) i (outFile build)
      case x of
        Left ierr   -> putStrLn $ ppInterpError ierr
        Right True  -> putStrLn "copied from cache"
        Right False -> putStrLn "made a new one"

main :: IO ()
main = do
 let opts = info (buildParser <**> helper)
       ( fullDesc
      <> progDesc "Give it a source file and an expression to render (which may refer to things declared in the source file), and it outputs an image, using hashing to avoid rerendering images unnecessarily."
      <> header "diagrams-builder - dynamically render diagrams" )

 build <- execParser opts

 case chooseBackendInfo build of
   Left err    -> die err
   Right binfo -> simple build binfo
