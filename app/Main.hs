{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           Control.Applicative   (many)
import           Data.ByteString.Char8 (ByteString)
import           Data.ComiCal          (imageCollections, imageIssues)
import           Data.ComiCal.Types    (mkCalendar)
import           Options.Applicative


main :: IO ()
main =
    do (collectionSlugs, issueSlugs) <- execParser opts
       print . foldr1 (<>) =<< mappend <$>
         mapM (fmap mkCalendar . imageIssues) issueSlugs <*>
         mapM (fmap mkCalendar . imageCollections) collectionSlugs


opts :: ParserInfo ([ByteString], [ByteString])
opts =
  info (args <**> helper) $
  fullDesc <>
  progDesc "Track the publish dates of comics" <>
  header "ComiCal - comics in your calendar"


args :: Parser ([ByteString], [ByteString])
args = (,) <$> collections <*> issues


collections :: Parser [ByteString]
collections =
  many $ strOption (
    long "collections" <>
    short 'c' <>
    metavar "SLUG" <>
    help "Track collected editions"
  )


issues :: Parser [ByteString]
issues =
  many $ strOption (
    long "issues" <>
    short 'i' <>
    metavar "SLUG" <>
    help "Track single issues"
  )
