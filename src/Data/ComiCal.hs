{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.ComiCal where

import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS
import           Data.Maybe            (catMaybes)
import           Data.Text             (Text)
import qualified Data.Text             as Text
import           Data.Text.Encoding    (decodeUtf8)
import           Data.Time
import           Network.HTTP.Req      (GET (..), NoReqBody (..),
                                        Scheme (Https), Url, bsResponse,
                                        defaultHttpConfig, https, req,
                                        responseBody, runReq, (/:))
import           Text.HTML.TagSoup
import           Text.URI              (URI)
import qualified Text.URI              as URI


data Series = Series
  { seriesTitle    :: ByteString
  , seriesURI      :: Url 'Https
  , seriesReleases :: [Release]
  }
  deriving (Eq, Show)


data Release = Release
  { releaseURI  :: URI
  , releaseDate :: Day
  }
  deriving (Eq)


instance Show Release where
    show (Release uri date) = unwords [ Text.unpack (URI.render uri), "on", show date ]


imageComic :: Text -> IO Series
imageComic slug = runReq defaultHttpConfig $
    do let url = https "imagecomics.com" /: "comics" /: "list" /: "series" /: slug /: "releases"
       tags <- parseTags . responseBody <$> req GET url NoReqBody bsResponse mempty
       pure $ Series (parseTitle tags) url (catMaybes (parseReleases tags))
  where
    parseTitle :: [Tag ByteString] -> ByteString
    parseTitle =
        BS.unwords .
        takeWhile (/= "Releases") . BS.words .
        fromTagText .
        head . filter isTagText .
        dropWhile (~/= ("<h2>" :: String))

    parseRelease :: [Tag ByteString] -> Maybe Release
    parseRelease release =
        Release <$> parseReleaseURI release <*> parseReleaseDate release

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

    parseReleases :: [Tag ByteString] -> [Maybe Release]
    parseReleases =
        map parseRelease .
        sections (~== ("<div class=\"cell u-mb1\">" :: String)) .
        dropWhile (~/= ("<section class=\"comics-grid u-pb2\">" :: String))
