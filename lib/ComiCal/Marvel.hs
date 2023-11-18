-- |
-- Module      : ComiCal.Marvel
-- Copyright   : (c) Eric Bailey, 2019-2021
-- License     : MIT
-- Maintainer  : eric@ericb.me
-- Stability   : experimental
-- Portability : POSIX
module ComiCal.Marvel (publisher) where

import ComiCal.App (Publisher (..))
import ComiCal.Types (Scraper (..), Series (..))
import ComiCal.Util (getHttps, parseReleases)
import Control.Arrow (second)
import Control.Monad.Reader (asks)
import qualified Data.Text as T
import Data.Time.Compat (defaultTimeLocale, parseTimeM)
import Text.URI (mkURI)
import Text.XML.Cursor

publisher :: Publisher
publisher =
  Publisher
    { scraper =
        Scraper
          { partitionReleases = \cursor ->
              cursor
                $// element "div"
                >=> attributeIs "class" "JCMultiRow  JCMultiRow-comic_issue"
                &// element "div"
                >=> attributeIs "class" "row-item-text",
            parseTitle = \cursor ->
              head . T.splitOn " | " . head $
                cursor
                  $// (element "meta" >=> attributeIs "property" "og:title")
                  &| head
                  . attribute "content",
            parseReleaseTitle = \cursor ->
              T.strip . head $
                cursor
                  $// element "h5"
                  &// element "a"
                  >=> attributeIs "class" "meta-title"
                  &/ content,
            parseReleaseDate = \cursor ->
              parseTimeM True defaultTimeLocale "%B %e, %Y" . T.unpack . last $
                cursor
                  $// element "div"
                  >=> attributeIs "class" "featured-item-meta"
                  &/ element "div"
                  &/ content
          },
      getCollections = fail "Not yet implemented",
      getIssues =
        do
          (seriesSlug, cfg) <- asks (second scraper)
          issuesURI <-
            mkURI $
              "https://www.marvel.com/comics/series/"
                <> seriesSlug
          tags <- getHttps issuesURI
          Series (parseTitle cfg tags) seriesSlug issuesURI <$> parseReleases tags
    }
