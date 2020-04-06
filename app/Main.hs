{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           Control.Applicative   (many)
import           Data.ByteString.Char8 (ByteString)
import qualified Data.ComiCal          as ComiCal
import           Data.ComiCal.Types    (mkCalendar)
import           Options.Applicative


data PullBox = PullBox
  { imageCollectionSlugs :: [ByteString]
  , imageIssueSlugs      :: [ByteString]
  , dcIssueSlugs         :: [ByteString] }
  deriving (Eq)


main :: IO ()
main =
    do pullBox <- execParser opts
       let pull = mapM . (fmap mkCalendar .)
       print . foldr1 (<>) =<<
         pull ComiCal.imageCollections (imageCollectionSlugs pullBox) <>
         pull ComiCal.imageIssues (imageIssueSlugs pullBox) <>
         pull ComiCal.dcIssues (dcIssueSlugs pullBox)


opts :: ParserInfo PullBox
opts =
  info (args <**> helper) $
  fullDesc <>
  progDesc "Track the publish dates of comics" <>
  header "ComiCal - comics in your calendar"


args :: Parser PullBox
args = PullBox <$> imageCollections <*> imageIssues <*> dcIssues


imageCollections :: Parser [ByteString]
imageCollections =
  many $ strOption (
    long "image-collections" <>
    metavar "SLUG" <>
    help "Track collected editions of Image comics"
  )


imageIssues :: Parser [ByteString]
imageIssues =
  many $ strOption (
    long "image-issues" <>
    metavar "SLUG" <>
    help "Track single issues of Image comics"
  )


dcIssues :: Parser [ByteString]
dcIssues =
  many $ strOption (
    long "dc-issues" <>
    metavar "SLUG" <>
    help "Track single issues of DC comics"
  )
