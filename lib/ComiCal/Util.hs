-- |
-- Module      : ComiCal.Util
-- Copyright   : (c) Eric Bailey, 2019-2021
-- License     : MIT
-- Maintainer  : eric@ericb.me
-- Stability   : experimental
-- Portability : POSIX
module ComiCal.Util
  ( getHttps,
    parseReleases,
  )
where

import ComiCal.App
import ComiCal.Types
import Control.Applicative ((<|>))
import Control.Lens (views, (.~))
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
import Text.URI (URI, mkScheme, mkURI, renderStr, unRText)
import Text.URI.Lens (uriPath, uriScheme)

getHttps :: MonadIO m => URI -> m [Tag ByteString]
getHttps theUri =
  do
    url <- maybe (error ("Failed to GET " <> renderStr theUri)) (pure . fst) (useHttpsURI theUri)
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

-- FIXME: This is messy and brittle.
parseRelease :: [Tag ByteString] -> ComiCalApp (Maybe Release)
parseRelease tags =
  do
    let maybeURI = (uriScheme .~ mkScheme "https") <$> parseReleaseURI tags
    case maybeURI of
      Nothing ->
        pure Nothing
      Just theURI ->
        do
          let lastPath = views uriPath (unRText . last) theURI
          (maybeReleaseNumber, finalTags) <- case parseReleaseNumber lastPath of
            Nothing ->
              pure (Nothing, tags)
            Just (Left n) ->
              pure (Just n, tags)
            Just (Right n) ->
              do
                finalTags <- getHttps theURI
                pure (Just n, finalTags)
          cfg <- asks (scraper . snd)
          let theTitle = parseReleaseTitle cfg tags
          mkRelease lastPath theTitle maybeReleaseNumber theURI finalTags

parseReleaseNumber :: Text -> Maybe (Either Int Int)
parseReleaseNumber lastPath = (Left <$> go "-") <|> (Right <$> go "_")
  where
    go sep = readMaybe (T.unpack (last (T.splitOn sep lastPath)))

parseReleaseURI :: [Tag ByteString] -> Maybe URI
parseReleaseURI tags =
  do
    anchorTag <- listToMaybe (dropWhile (~/= ("<a>" :: String)) tags)
    mkURI (decodeUtf8 (fromAttrib "href" anchorTag))

mkRelease :: Text -> ByteString -> Maybe Int -> URI -> [Tag ByteString] -> ComiCalApp (Maybe Release)
mkRelease lastPath theTitle maybeReleaseNumber theURI tags =
  do
    cfg <- asks (scraper . snd)
    pure $
      Release (BS.pack (T.unpack lastPath)) theTitle maybeReleaseNumber theURI
        <$> parseReleaseDate cfg tags
