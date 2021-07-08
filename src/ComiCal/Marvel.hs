module ComiCal.Marvel where

import ComiCal.App
import ComiCal.Types
import ComiCal.Util
import Control.Arrow (second)
import Control.Monad.Reader
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.List (find)
import qualified Data.List.NonEmpty as NE
import Data.Maybe (catMaybes)
import qualified Data.Text as T
import Data.Time.Format.Compat
import Text.HTML.TagSoup
import Text.URI

publisher :: Publisher
publisher =
  Publisher
    { scraper =
        Scraper
          { partitionReleases =
              concatMap (partitions (~== ("<div class=\"row-item-text\">" :: String)))
                . partitions (~== ("<div class=\"row-item comic-item\">" :: String))
                . dropWhile (~/= ("<div class=\"JCMultiRow  JCMultiRow-comic_issue\">" :: String)),
            parseTitle =
              fromTagText
                . head
                . filter isTagText
                . dropWhile (~/= ("<h1>" :: String)),
            parseReleaseTitle =
              BS.strip . fromTagText . head . filter isTagText
                . dropWhile (~/= ("<h5>" :: String)),
            parseReleaseDate = \tags ->
              do
                tag <-
                  find isTagText $
                    head $
                      tail $
                        tail $
                          partitions (~== ("<div>" :: String)) $
                            dropWhile (~/= ("<div class=\"featured-item-meta\">" :: String)) tags
                let input = BS.unpack (fromTagText tag)
                parseTimeM True defaultTimeLocale "%B %e, %Y" input
          },
      getCollections = fail "Not yet implemented",
      getIssues =
        do
          (seriesSlug, cfg) <- asks (second scraper)
          issuesURI <-
            mkURI $
              "https://www.marvel.com/comics/series/"
                <> T.pack (BS.unpack seriesSlug)
          tags <- liftIO $ getSeries issuesURI
          Series (parseTitle cfg tags) seriesSlug issuesURI <$> parseMarvelReleases tags
    }

parseMarvelReleases :: [Tag ByteString] -> ComiCalApp (NE.NonEmpty Release)
parseMarvelReleases tags =
  asks (partitionReleases . scraper . snd) <*> pure tags
    >>= fmap (NE.fromList . catMaybes)
      . mapM parseMarvelRelease

parseMarvelRelease :: [Tag ByteString] -> ComiCalApp (Maybe Release)
parseMarvelRelease seriesTags =
  do
    (seriesSlug, cfg) <- asks (second scraper)
    let theTitle = parseReleaseTitle cfg seriesTags
    -- FIXME: MaybeT?
    Just theURI_ <- pure $ parseReleaseURI seriesTags
    let theURI = theURI_ {uriScheme = mkScheme "https"}
    -- FIXME: MaybeT?
    Just lastPath <- pure $ unRText . NE.last . snd <$> uriPath theURI
    let n = parseReleaseNumber (last (BS.split '/' seriesSlug)) lastPath "_"
    -- FIXME: rename getSeris
    releaseTags <- getSeries theURI
    pure $
      Release (BS.pack (T.unpack lastPath)) theTitle n theURI
        <$> parseReleaseDate cfg releaseTags

-- wip =
--   do
--     rawRelease <- head . partitionReleases (scraper publisher) <$> bw
--     runReaderT (runApp (parseMarvelRelease rawRelease)) ("29700/black_widow_2020", publisher)

-- flip runReaderT ("slug", publisher) .
-- runApp . parseMarvelRelease .

bw :: IO [Tag ByteString]
bw =
  do
    file <- BS.readFile "./data/blackwidow.html"
    pure $ parseTags file
