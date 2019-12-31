{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Data.ComiCal.Parser where

import           Data.ComiCal.Types

import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS
import qualified Data.List.NonEmpty    as NE
import           Data.Maybe            (catMaybes, listToMaybe)
import           Data.Text             (Text)
import qualified Data.Text             as Text
import           Data.Text.Encoding    (decodeUtf8)
import           Data.Time             (Day, defaultTimeLocale, parseTimeM)
import           Text.HTML.TagSoup     (Tag, fromAttrib, fromTagText, isTagText,
                                        sections, (~/=), (~==))
import           Text.URI              (URI)
import qualified Text.URI              as URI


parseSeries :: Text -> URI -> [Tag ByteString] -> Series
parseSeries slug theURI tags =
    Series (parseTitle tags) theURI (catMaybes (parseReleases slug tags))


parseTitle :: [Tag ByteString] -> ByteString
parseTitle =
    BS.unwords .
    takeWhile (/= "Releases") . BS.words .
    fromTagText .
    head . filter isTagText .
    dropWhile (~/= ("<h2>" :: String))


parseRelease :: Text -> [Tag ByteString] -> Maybe Release
parseRelease slug tags =
  do theURI <- parseReleaseURI tags
     lastPath <- URI.unRText . NE.last . snd <$> URI.uriPath theURI
     n <- maybeRead . Text.unpack =<< Text.stripPrefix (slug <> "-") lastPath
     Release n theURI <$> parseReleaseDate tags


parseReleaseURI :: [Tag ByteString] -> Maybe URI
parseReleaseURI =
    URI.mkURI .
    decodeUtf8 .
    fromAttrib "href" .
    head . dropWhile (~/= ("<a>" :: String))


parseReleaseDate :: [Tag ByteString] -> Maybe Day
parseReleaseDate =
    parseTimeM True defaultTimeLocale "%b %e, %Y" .
    BS.unpack .
    fromTagText .
    head . filter isTagText .
    dropWhile (~/= ("<span class=date>" :: String))


parseReleases :: Text -> [Tag ByteString] -> [Maybe Release]
parseReleases slug =
    map (parseRelease slug) .
    sections (~== ("<div class=\"cell u-mb1\">" :: String)) .
    dropWhile (~/= ("<section class=\"comics-grid u-pb2\">" :: String))


maybeRead :: Read a => String -> Maybe a
maybeRead = fmap fst . listToMaybe . reads
