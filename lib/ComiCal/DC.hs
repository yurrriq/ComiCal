-- |
-- Module      : ComiCal.DC
-- Copyright   : (c) Eric Bailey, 2019-2024
-- License     : MIT
-- Maintainer  : eric@ericb.me
-- Stability   : experimental
-- Portability : POSIX
module ComiCal.DC (publisher) where

import ComiCal.App (Publisher (..))
import ComiCal.Types (Scraper (..), Series (..))
import ComiCal.Util (getHttps, parseReleases, urlEncode)
import Control.Arrow (second)
import Control.Monad.Reader (asks)
import qualified Data.Text as T
import Data.Time.Compat (localDay, zonedTimeToLocalTime)
import Data.Time.Format.ISO8601.Compat (iso8601ParseM)
import Text.URI (mkURI)
import Text.XML.Cursor

publisher :: Publisher
publisher =
  Publisher
    { scraper =
        Scraper
          { partitionReleases = \cursor ->
              filter (not . null . ($// element "div")) $
                cursor
                  $// element "tbody"
                  &// element "td",
            parseTitle = \cursor ->
              T.strip . head $
                cursor
                  $// (element "meta" >=> attributeIs "property" "og:title")
                  &| head
                  . attribute "content",
            parseReleaseTitle = \cursor ->
              T.strip . head $
                cursor
                  $// element "div"
                  >=> attributeIs "class" "views-field views-field-title"
                  &// element "span"
                  &// content,
            parseReleaseDate = \cursor ->
              fmap (localDay . zonedTimeToLocalTime) . iso8601ParseM . T.unpack . head $
                cursor
                  $// element "span"
                  >=> attributeIs "class" "date-display-single"
                  >=> attribute "content"
          },
      getCollections = fail "Not yet implemented",
      getIssues =
        do
          (seriesSlug, cfg) <- asks (second scraper)
          issuesURI <-
            mkURI $ "https://www.dccomics.com/comics/" <> urlEncode seriesSlug
          tags <- getHttps issuesURI
          Series (parseTitle cfg tags) seriesSlug issuesURI <$> parseReleases tags
    }
