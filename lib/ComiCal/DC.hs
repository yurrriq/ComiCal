-- |
-- Module      : ComiCal.DC
-- Copyright   : (c) Eric Bailey, 2019-2021
-- License     : MIT
-- Maintainer  : eric@ericb.me
-- Stability   : experimental
-- Portability : POSIX
module ComiCal.DC (publisher) where

import ComiCal.App (Publisher (..))
import ComiCal.Types (Scraper (..), Series (..))
import ComiCal.Util (getHttps, parseReleases)
import Control.Arrow (second)
import Control.Monad.Reader (asks)
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.Text as T
import Data.Time.Compat (localDay, zonedTimeToLocalTime)
import Data.Time.Format.ISO8601.Compat (iso8601ParseM)
import Text.HTML.TagSoup
  ( fromAttrib,
    fromTagText,
    isTagText,
    partitions,
    (~/=),
    (~==),
  )
import Text.URI (mkURI)

publisher :: Publisher
publisher =
  Publisher
    { scraper =
        Scraper
          { partitionReleases =
              concatMap (partitions (~== ("<span class=\"field-content\">" :: String)))
                . partitions (~== ("<div class=\"views-field views-field-title\">" :: String))
                . dropWhile (~/= ("<tbody>" :: String)),
            parseTitle =
              LBS.unwords . takeWhile (/= "Releases") . LBS.words
                . fromTagText
                . head
                . filter isTagText
                . dropWhile (~/= ("<div id=\"content-area\">" :: String)),
            parseReleaseTitle =
              fromTagText . head . filter isTagText
                . dropWhile (~/= ("<span>" :: String)),
            parseReleaseDate =
              fmap (localDay . zonedTimeToLocalTime) . iso8601ParseM
                . LBS.unpack
                . fromAttrib "content"
                . head
                . dropWhile (~/= ("<span property=\"schema:datePublished\">" :: String))
          },
      getCollections = fail "Not yet implemented",
      getIssues =
        do
          (seriesSlug, cfg) <- asks (second scraper)
          issuesURI <-
            mkURI $
              "https://www.dccomics.com/comics/"
                <> T.pack (LBS.unpack seriesSlug)
          tags <- getHttps issuesURI
          Series (parseTitle cfg tags) seriesSlug issuesURI <$> parseReleases tags
    }
