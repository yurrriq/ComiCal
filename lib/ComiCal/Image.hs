{-# LANGUAGE DataKinds #-}

-- |
-- Module      : ComiCal.Image
-- Copyright   : (c) Eric Bailey, 2019-2024
-- License     : MIT
-- Maintainer  : eric@ericb.me
-- Stability   : experimental
-- Portability : POSIX
module ComiCal.Image (publisher) where

import ComiCal.App (Publisher (..))
import ComiCal.Types (Scraper (..), Series (..))
import ComiCal.Util (getHttps, parseReleases, urlEncode)
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
              cursor $// element "div" >=> attributeIs "class" "cell  u-mb1",
            parseTitle = \cursor ->
              T.strip . last . T.splitOn " | " . head $
                cursor
                  $// (element "meta" >=> attributeIs "property" "og:title")
                  &| head
                  . attribute "content",
            parseReleaseTitle = \cursor ->
              T.strip . head $
                cursor
                  $// element "a"
                  &// element "span"
                  &/ content,
            parseReleaseDate = \cursor ->
              parseTimeM True defaultTimeLocale "%b %e, %Y" . T.unpack . head $
                cursor
                  $// element "span"
                  >=> attributeIs "class" "date"
                  &/ content
          },
      getCollections =
        do
          (seriesSlug, cfg) <- asks (second scraper)
          collectionsURI <-
            mkURI $
              "https://imagecomics.com/comics/list/series/"
                <> urlEncode seriesSlug
                <> "/collected-editions"
          tags <- getHttps collectionsURI
          Series (parseTitle cfg tags) seriesSlug collectionsURI <$> parseReleases tags,
      getIssues =
        do
          (seriesSlug, cfg) <- asks (second scraper)
          issuesURI <-
            mkURI $
              "https://imagecomics.com/comics/list/series/"
                <> urlEncode seriesSlug
                <> "/releases"
          tags <- getHttps issuesURI
          Series (parseTitle cfg tags) seriesSlug issuesURI <$> parseReleases tags
    }
