module Data.ComiCal.DC where

import Control.Monad.Reader (MonadIO (liftIO), MonadReader (ask))
import qualified Data.ByteString.Char8 as BS
import Data.ComiCal.App (ComiCalApp, ComiCalConfig (..))
import Data.ComiCal.Types (Series (..))
import Data.ComiCal.Util (getSeries, parseReleases)
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

config :: ComiCalConfig
config =
  ComiCalConfig
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
    }

-- | Given a [DC Comics series](https://www.dccomics.com/comics) slug,
-- fetch, parse, and return the issues of the 'Series'.
getIssues :: ComiCalApp Series
getIssues =
  do
    (seriesSlug, cfg) <- ask
    issuesURI <-
      mkURI $
        "https://www.dccomics.com/comics/"
          <> T.pack (BS.unpack seriesSlug)
    tags <- liftIO $ getSeries issuesURI
    Series (parseTitle cfg tags) seriesSlug issuesURI <$> parseReleases tags
