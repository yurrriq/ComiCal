{-# LANGUAGE DataKinds #-}

-- |
-- Module      : ComiCal.Image
-- Copyright   : (c) Eric Bailey, 2019-2021
-- License     : MIT
-- Maintainer  : eric@ericb.me
-- Stability   : experimental
-- Portability : POSIX
module ComiCal.Image (publisher) where

import ComiCal.App (Publisher (..))
import ComiCal.Types (Scraper (..), Series (..))
import ComiCal.Util (getHttps, parseReleases)
import Control.Arrow (second)
import Control.Monad.Reader (asks)
import qualified Data.ByteString.Char8 as BS
import Data.List (find)
import qualified Data.Text as T
import Data.Time.Compat (defaultTimeLocale, parseTimeM)
import Text.HTML.TagSoup
  ( fromTagText,
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
              partitions (~== ("<div class=\"cell u-mb1\">" :: String))
                . dropWhile (~/= ("<section class=\"comics-grid u-pb2\">" :: String)),
            parseTitle =
              BS.unwords . takeWhile (/= "Releases") . BS.words
                . fromTagText
                . head
                . filter isTagText
                . dropWhile (~/= ("<h2>" :: String)),
            parseReleaseTitle =
              fromTagText . head . filter isTagText
                . dropWhile (~/= ("<span>" :: String)),
            parseReleaseDate = \tags ->
              do
                spanTag <-
                  find isTagText $
                    dropWhile (~/= ("<span class=date>" :: String)) tags
                let input = BS.unpack (fromTagText spanTag)
                parseTimeM True defaultTimeLocale "%b %e, %Y" input
          },
      getCollections =
        do
          (seriesSlug, cfg) <- asks (second scraper)
          collectionsURI <-
            mkURI $
              "https://imagecomics.com/comics/list/series/"
                <> T.pack (BS.unpack seriesSlug)
                <> "/collected-editions"
          tags <- getHttps collectionsURI
          Series (parseTitle cfg tags) seriesSlug collectionsURI <$> parseReleases tags,
      getIssues =
        do
          (seriesSlug, cfg) <- asks (second scraper)
          issuesURI <-
            mkURI $
              "https://imagecomics.com/comics/list/series/"
                <> T.pack (BS.unpack seriesSlug)
                <> "/releases"
          tags <- getHttps issuesURI
          Series (parseTitle cfg tags) seriesSlug issuesURI <$> parseReleases tags
    }
