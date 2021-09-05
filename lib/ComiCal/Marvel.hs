-- |
-- Module      : ComiCal.Marvel
-- Copyright   : (c) Eric Bailey, 2019-2021
-- License     : MIT
-- Maintainer  : eric@ericb.me
-- Stability   : experimental
-- Portability : POSIX
module ComiCal.Marvel (publisher) where

import ComiCal.App
import ComiCal.Types
import ComiCal.Util
import Control.Arrow (second)
import Control.Monad.Reader
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.List (find)
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
                . takeWhile (~/= ("<section>" :: String))
                . dropWhile (~/= ("<div class=\"JCMultiRow  JCMultiRow-comic_issue\">" :: String)),
            parseTitle =
              fromTagText
                . head
                . filter isTagText
                . dropWhile (~/= ("<h1>" :: String)),
            parseReleaseTitle =
              -- FIXME: strip
              fromTagText . head . filter isTagText
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
                let input = LBS.unpack (fromTagText tag)
                parseTimeM True defaultTimeLocale "%B %e, %Y" input
          },
      getCollections = fail "Not yet implemented",
      getIssues =
        do
          (seriesSlug, cfg) <- asks (second scraper)
          issuesURI <-
            mkURI $
              "https://www.marvel.com/comics/series/"
                <> T.pack (LBS.unpack seriesSlug)
          tags <- getHttps issuesURI
          Series (parseTitle cfg tags) seriesSlug issuesURI <$> parseReleases tags
    }
