module Data.ComiCal.Util where

import Control.Monad.Reader
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.ComiCal.App
import Data.ComiCal.Types
import qualified Data.List.NonEmpty as NE
import Data.Maybe (catMaybes, listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import Network.HTTP.Req
import Text.HTML.TagSoup
import Text.Read (readMaybe)
import Text.URI
import qualified Text.URI as URI

parseReleases :: [Tag ByteString] -> ComiCalApp (NE.NonEmpty Release)
parseReleases tags =
  asks (partitionReleases . snd) <*> pure tags
    >>= fmap (NE.fromList . catMaybes)
      . mapM parseRelease

parseRelease :: [Tag ByteString] -> ComiCalApp (Maybe Release)
parseRelease tags =
  do
    (seriesSlug, cfg) <- ask
    let theTitle = parseReleaseTitle cfg tags
    Just theURI <- pure $ parseReleaseURI tags
    Just lastPath <- pure $ URI.unRText . NE.last . snd <$> URI.uriPath theURI
    let n = parseReleaseNumber seriesSlug lastPath "-"
    pure $ Release (BS.pack (T.unpack lastPath)) theTitle n theURI <$> parseReleaseDate cfg tags

parseReleaseNumber :: ByteString -> Text -> Text -> Maybe Int
parseReleaseNumber seriesSlug lastPath sep =
  do
    let prefix = T.pack (BS.unpack seriesSlug) <> sep
    readMaybe . T.unpack =<< T.stripPrefix prefix lastPath

parseReleaseURI :: [Tag ByteString] -> Maybe URI
parseReleaseURI tags =
  do
    anchorTag <- listToMaybe (dropWhile (~/= ("<a>" :: String)) tags)
    URI.mkURI (decodeUtf8 (fromAttrib "href" anchorTag))

-- getSeries :: MonadHttp m => URI -> m [Tag ByteString]
getSeries :: MonadIO m => URI -> m [Tag ByteString]
getSeries theUri =
  do
    url <- maybe (error "Failed to GET issues") (pure . fst) (useHttpsURI theUri)
    res <-
      liftIO $
        runReq defaultHttpConfig $
          req GET url NoReqBody bsResponse mempty
    pure (parseTags (responseBody res))
