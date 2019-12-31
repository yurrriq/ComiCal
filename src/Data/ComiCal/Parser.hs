{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Data.ComiCal.Parser where

import           Data.ComiCal.Types

import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS
import qualified Data.List.NonEmpty    as NE
import           Data.Maybe            (catMaybes, listToMaybe)
import qualified Data.Text             as Text
import           Data.Text.Encoding    (decodeUtf8)
import           Data.Time             (Day, defaultTimeLocale, parseTimeM)
import           Text.HTML.TagSoup     (Tag, fromAttrib, fromTagText, isTagText,
                                        sections, (~/=), (~==))
import           Text.URI              (URI)
import qualified Text.URI              as URI


parseSeries :: ByteString -> URI -> [Tag ByteString] -> Series
parseSeries theSlug theURI tags =
    Series (parseTitle tags) theSlug theURI $
    parseReleases theSlug tags


parseTitle :: [Tag ByteString] -> ByteString
parseTitle =
    BS.unwords .
    takeWhile (/= "Releases") . BS.words .
    fromTagText .
    head . filter isTagText .
    dropWhile (~/= ("<h2>" :: String))


parseRelease :: ByteString -> [Tag ByteString] -> Maybe Release
parseRelease theSlug tags =
  do theURI <- parseReleaseURI tags
     lastPath <- URI.unRText . NE.last . snd <$> URI.uriPath theURI
     let prefix = BS.unpack theSlug <> "-"
     n <- maybeRead . Text.unpack =<<
          Text.stripPrefix (Text.pack prefix) lastPath
     let relSlug = prefix <> show n
     Release n (BS.pack relSlug) theURI <$> parseReleaseDate tags


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


parseReleases :: ByteString -> [Tag ByteString] -> NE.NonEmpty Release
parseReleases theSlug =
    NE.fromList . catMaybes .
    map (parseRelease theSlug) .
    sections (~== ("<div class=\"cell u-mb1\">" :: String)) .
    dropWhile (~/= ("<section class=\"comics-grid u-pb2\">" :: String))


maybeRead :: Read a => String -> Maybe a
maybeRead = fmap fst . listToMaybe . reads
