module ComiCal.DC where

import ComiCal.App (Publisher (..))
import ComiCal.Types (Scraper (..), Series (..))
import ComiCal.Util (getHttps, parseReleases)
import Control.Arrow (second)
import Control.Monad.Reader (MonadIO (liftIO), asks)
import qualified Data.ByteString.Char8 as BS
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
              BS.unwords . takeWhile (/= "Releases") . BS.words
                . fromTagText
                . head
                . filter isTagText
                . dropWhile (~/= ("<div id=\"content-area\">" :: String)),
            parseReleaseTitle =
              fromTagText . head . filter isTagText
                . dropWhile (~/= ("<span>" :: String)),
            parseReleaseDate =
              fmap (localDay . zonedTimeToLocalTime) . iso8601ParseM
                . BS.unpack
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
                <> T.pack (BS.unpack seriesSlug)
          tags <- liftIO $ getHttps issuesURI
          Series (parseTitle cfg tags) seriesSlug issuesURI <$> parseReleases tags
    }
