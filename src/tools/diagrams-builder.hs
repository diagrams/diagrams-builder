{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards    #-}

import           Diagrams.Backend
import           Diagrams.Builder
import           Diagrams.Builder.Opts
import           Diagrams.Prelude hiding (simple)
import           Options.Applicative

-- | Record of command-line options.
data Build = Build
  { buildSize :: SizeSpec V2 Int
  , cacheDir  :: FilePath
  , srcFile   :: FilePath
  , outFile   :: FilePath
  , bExpr     :: String
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

-- | Make a snippet from the command line arguments.
buildSnippet :: Build -> IO Snippets
buildSnippet Build {..} = do
  src <- readFile srcFile
  return $ emptySnippet &~ do
    snippets .= [src]
    imports  .= ["Prelude", "Diagrams.Prelude", "Diagrams.Backend", "Geometry"
                , "Diagrams.Backend.PGF"]
    pragmas  .= ["GADTs", "NoMonomorphismRestriction", "FlexibleContexts"]

-- | A simple builder given some backend info.
simple :: Build -> BackendInfo -> IO ()
simple build info = do
  snippet <- buildSnippet build
  let builder = DiagramBuilder
        { _diaInfo    = pgfInfo
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
      <> progDesc "Print a greeting for TARGET"
      <> header "hello - a test for optparse-applicative" )

 build <- execParser opts

 simple build pgfInfo

