-- |
-- Module      : Main
-- Description : The main entrypoint for the ComiCal executable
-- Copyright   : (c) Eric Bailey, 2019-2021
-- License     : MIT
-- Maintainer  : eric@ericb.me
-- Stability   : stable
-- Portability : POSIX
module Main (main) where

import qualified ComiCal
import ComiCal.Types (PullList (..))
import Control.Monad.Trans (liftIO)
import Data.Aeson (decodeFileStrict)
import Options.Applicative

main :: IO ()
main =
  do
    appMode <- execParser opts
    pullList <- case appMode of
      FromFile fname ->
        liftIO $
          decodeFileStrict fname
            >>= maybe (error ("Failed to parse " <> fname)) pure
      FromFlags pullList -> pure pullList
    let pull = mapM . (fmap ComiCal.mkCalendar .)
    print . foldr1 (<>)
      =<< pull ComiCal.dcIssues (dcIssues pullList)
      <> pull ComiCal.imageCollections (imageCollections pullList)
      <> pull ComiCal.imageIssues (imageIssues pullList)
      <> pull ComiCal.marvelIssues (marvelIssues pullList)

data AppMode
  = FromFile FilePath
  | FromFlags PullList

opts :: ParserInfo AppMode
opts =
  info (mkAppMode <**> helper) $
    fullDesc
      <> progDesc "Track the publish dates of comics"
      <> header "ComiCal - comics in your calendar"

mkAppMode :: Parser AppMode
mkAppMode = FromFile <$> config <|> FromFlags <$> fromFlags
  where
    config =
      strOption
        ( long "pull-list" <> short 'p'
            <> metavar "PULL_LIST"
            <> help "Pull list file (JSON)"
        )

fromFlags :: Parser PullList
fromFlags =
  PullList
    <$> many
      ( strOption
          ( long "dc-collections"
              <> metavar "SLUG"
              <> help "Track collected editions of DC comics"
          )
      )
      <*> many
        ( strOption
            ( long "dc-issues"
                <> metavar "SLUG"
                <> help "Track single issues of DC comics"
            )
        )
      <*> many
        ( strOption
            ( long "image-collections"
                <> metavar "SLUG"
                <> help "Track collected editions of Image comics"
            )
        )
      <*> many
        ( strOption
            ( long "image-issues"
                <> metavar "SLUG"
                <> help "Track single issues of Image comics"
            )
        )
      <*> many
        ( strOption
            ( long "marvel-collections"
                <> metavar "SLUG"
                <> help "Track collected editions of Marvel comics"
            )
        )
      <*> many
        ( strOption
            ( long "marvel-issues"
                <> metavar "SLUG"
                <> help "Track single issues of Marvel comics"
            )
        )
