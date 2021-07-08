module ComiCal.Util where

import ComiCal.App
import ComiCal.Types
import Control.Arrow (second)
import Control.Monad.Reader
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS
import qualified Data.List.NonEmpty as NE
import Data.Maybe (catMaybes, listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import Network.HTTP.Req
import Text.HTML.TagSoup
import Text.Read (readMaybe)
import Text.URI

getHttps :: MonadIO m => URI -> m [Tag ByteString]
getHttps theUri =
  do
    url <- maybe (error ("Failed to GET " <> show theUri)) (pure . fst) (useHttpsURI theUri)
    res <-
      liftIO $
        runReq defaultHttpConfig $
          req GET url NoReqBody bsResponse mempty
    pure (parseTags (responseBody res))

parseReleases :: [Tag ByteString] -> ComiCalApp (NE.NonEmpty Release)
parseReleases tags =
  asks (partitionReleases . scraper . snd) <*> pure tags
    >>= fmap (NE.fromList . catMaybes)
      . mapM parseRelease

parseRelease :: [Tag ByteString] -> ComiCalApp (Maybe Release)
parseRelease tags =
  do
    (seriesSlug, cfg) <- asks (second scraper)
    let theTitle = parseReleaseTitle cfg tags
    -- FIXME: MaybeT?
    Just theURI_ <- pure $ parseReleaseURI tags
    let theURI = theURI_ {uriScheme = mkScheme "https"}
    -- FIXME: MaybeT?
    Just lastPath <- pure $ unRText . NE.last . snd <$> uriPath theURI
    case parseReleaseNumber seriesSlug lastPath "-" of
      Just n ->
        mkRelease lastPath theTitle n theURI tags
      Nothing ->
        do
          let Just n = parseReleaseNumber (last (BS.split '/' seriesSlug)) lastPath "_"
          mkRelease lastPath theTitle n theURI =<< getHttps theURI

parseReleaseNumber :: ByteString -> Text -> Text -> Maybe Int
parseReleaseNumber seriesSlug lastPath sep =
  do
    let prefix = T.pack (BS.unpack seriesSlug) <> sep
    readMaybe . T.unpack =<< T.stripPrefix prefix lastPath

parseReleaseURI :: [Tag ByteString] -> Maybe URI
parseReleaseURI tags =
  do
    anchorTag <- listToMaybe (dropWhile (~/= ("<a>" :: String)) tags)
    mkURI (decodeUtf8 (fromAttrib "href" anchorTag))

mkRelease :: Text -> ByteString -> Int -> URI -> [Tag ByteString] -> ComiCalApp (Maybe Release)
mkRelease lastPath theTitle n theURI tags =
  do
    cfg <- asks (scraper . snd)
    pure $
      Release (BS.pack (T.unpack lastPath)) theTitle (Just n) theURI
        <$> parseReleaseDate cfg tags
