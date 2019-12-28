{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Data.ComiCal where

import           Control.Lens          (makeLenses, (^.))
import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS
import qualified Data.List.NonEmpty    as NE
import           Data.Maybe            (catMaybes)
import           Data.Text             (Text)
import qualified Data.Text             as Text
import           Data.Text.Encoding    (decodeUtf8)
import           Data.Time             (Day, defaultTimeLocale, parseTimeM)
import           Network.HTTP.Req      (GET (..), NoReqBody (..),
                                        Scheme (Https), Url, bsResponse,
                                        defaultHttpConfig, https, req,
                                        responseBody, runReq, (/:))
import           Text.HTML.TagSoup     (Tag, fromAttrib, fromTagText, isTagText,
                                        parseTags, sections, (~/=), (~==))
import           Text.Printf           (printf)
import           Text.URI              (URI)
import qualified Text.URI              as URI


data Release = Release
  { _issue      :: Int
  , _releaseURI :: URI
  , _date       :: Day }
  deriving (Eq)

makeLenses ''Release

instance Show Release where
    show release =
      printf "#%d on %s" (release^.issue) (show (release^.date))


data Series = Series
  { _title     :: ByteString
  , _seriesURI :: Url 'Https
  , _releases  :: [Release] }
  deriving (Eq)

makeLenses ''Series

instance Show Series where
  show series = printf "%s (%d releases)"
                (BS.unpack (series^.title))
                (length (series^.releases))


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
        do uri <- parseReleaseURI release
           lastPath <- NE.last . snd <$> URI.uriPath uri
           -- FIXME: https://imagecomics.com/comics/releases/gideon-falls-1-directors-cut
           -- FIXME: https://imagecomics.com/comics/releases/medieval-spawn-witchblade-4-of-4
           let n = read .
                   reverse . takeWhile (/= '-') . reverse .
                   Text.unpack . URI.unRText $
                   lastPath
           Release n uri <$> parseReleaseDate release


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
