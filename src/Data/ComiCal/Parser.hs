{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.ComiCal.Parser
  ( parseSeries,
  )
where

import Control.Applicative ((<|>))
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.ComiCal.Types
import Data.List (find)
import qualified Data.List.NonEmpty as NE
import Data.Maybe (listToMaybe, mapMaybe)
import qualified Data.Text as Text
import Data.Text.Encoding (decodeUtf8)
import Data.Time.Compat
  ( Day,
    defaultTimeLocale,
    localDay,
    parseTimeM,
    zonedTimeToLocalTime,
  )
import Data.Time.Format.ISO8601.Compat (iso8601ParseM)
import Text.HTML.TagSoup
  ( Tag,
    fromAttrib,
    fromTagText,
    isTagText,
    partitions,
    (~/=),
    (~==),
  )
import Text.URI (URI)
import qualified Text.URI as URI

-- | Given a 'slug', 'URI', and a list of 'Tag's, parse and return the 'Series'.
parseSeries :: ByteString -> URI -> [Tag ByteString] -> Series
parseSeries theSlug theURI tags =
  Series (parseTitle tags) theSlug theURI $
    parseReleases theSlug tags

parseTitle :: [Tag ByteString] -> ByteString
parseTitle tags =
  BS.unwords . takeWhile (/= "Releases") . BS.words
    . fromTagText
    . head
    . filter isTagText
    $ (bar (foo tags) <|> bar tags)
  where
    bar = dropWhile (~/= ("<h2>" :: String))
    foo = dropWhile (~/= ("<div id=\"content-area\">" :: String))

parseRelease :: ByteString -> [Tag ByteString] -> Maybe Release
parseRelease seriesSlug tags =
  do
    let theTitle = parseReleaseTitle tags
    theURI <- parseReleaseURI tags
    lastPath <- URI.unRText . NE.last . snd <$> URI.uriPath theURI
    let n = parseReleaseNumber seriesSlug lastPath
    Release (BS.pack (Text.unpack lastPath)) theTitle n theURI <$> parseReleaseDate tags

parseReleaseNumber :: ByteString -> Text.Text -> Maybe Int
parseReleaseNumber seriesSlug lastPath =
  do
    let prefix = BS.unpack seriesSlug <> "-"
    maybeRead . Text.unpack
      =<< Text.stripPrefix (Text.pack prefix) lastPath

parseReleaseURI :: [Tag ByteString] -> Maybe URI
parseReleaseURI tags =
  do
    anchorTag <- listToMaybe (dropWhile (~/= ("<a>" :: String)) tags)
    URI.mkURI (decodeUtf8 (fromAttrib "href" anchorTag))

parseReleaseDate :: [Tag ByteString] -> Maybe Day
parseReleaseDate tags = parseImageDate tags <|> parseDCDate tags

parseImageDate :: [Tag ByteString] -> Maybe Day
parseImageDate tags =
  do
    spanTag <-
      find isTagText $
        dropWhile (~/= ("<span class=date>" :: String)) tags
    let input = BS.unpack (fromTagText spanTag)
    parseTimeM True defaultTimeLocale "%b %e, %Y" input

parseDCDate :: [Tag ByteString] -> Maybe Day
parseDCDate =
  fmap (localDay . zonedTimeToLocalTime) . iso8601ParseM
    . BS.unpack
    . fromAttrib "content"
    . head
    . dropWhile (~/= ("<span property=\"schema:datePublished\">" :: String))

parseReleaseTitle :: [Tag ByteString] -> ByteString
parseReleaseTitle =
  fromTagText . head . filter isTagText
    . dropWhile (~/= ("<span>" :: String))

parseReleases :: ByteString -> [Tag ByteString] -> NE.NonEmpty Release
parseReleases theSlug tags =
  NE.fromList . mapMaybe (parseRelease theSlug) $
    (partitionImageReleases tags <|> partitionDCReleases tags)

partitionImageReleases :: [Tag ByteString] -> [[Tag ByteString]]
partitionImageReleases =
  partitions (~== ("<div class=\"cell u-mb1\">" :: String))
    . dropWhile (~/= ("<section class=\"comics-grid u-pb2\">" :: String))

partitionDCReleases :: [Tag ByteString] -> [[Tag ByteString]]
partitionDCReleases =
  concatMap (partitions (~== ("<span class=\"field-content\">" :: String)))
    . partitions (~== ("<div class=\"views-field views-field-title\">" :: String))
    . dropWhile (~/= ("<tbody>" :: String))

maybeRead :: Read a => String -> Maybe a
maybeRead = fmap fst . listToMaybe . reads
