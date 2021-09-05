{-# LANGUAGE TupleSections #-}

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
import Control.Monad.Catch (Exception, MonadThrow, displayException, throwM)
import Control.Monad.Logger
import Control.Monad.Reader
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS
import qualified Data.List.NonEmpty as NE
import Data.Maybe (catMaybes, listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import Network.HTTP.Client (HttpException (..))
import Network.HTTP.Req
import Text.HTML.TagSoup
import Text.Read (readMaybe)
import Text.URI (URI, mkScheme, mkURI, render, renderStr, unRText)
import Text.URI.Lens (uriPath, uriScheme)

getHttps :: (MonadIO m, MonadLogger m, MonadThrow m) => URI -> m [Tag ByteString]
getHttps theURI =
  do
    $(logDebug) $ "GET " <> render theURI
    url <- maybe badURI (pure . fst) (useHttpsURI theURI)
    res <- runReq defaultHttpConfig $ req GET url NoReqBody bsResponse mempty
    pure (parseTags (responseBody res))
  where
    badURI = throwInvalidUrlM (renderStr theURI) "Unable to construct Network.HTTP.Req.Url"

parseReleases :: [Tag ByteString] -> ComiCalApp (NE.NonEmpty Release)
parseReleases tags =
  asks (partitionReleases . scraper . snd) <*> pure tags
    >>= fmap (NE.fromList . catMaybes)
      . mapM parseRelease

-- FIXME: This is messy and brittle.
parseRelease :: [Tag ByteString] -> ComiCalApp (Maybe Release)
parseRelease tags =
  do
    theURI <- (uriScheme .~ mkScheme "https") <$> parseReleaseURI tags
    let lastPath = views uriPath (unRText . last) theURI
    (maybeReleaseNumber, finalTags) <- case parseReleaseNumber lastPath of
      Nothing ->
        pure (Nothing, tags)
      Just (Left n) ->
        pure (Just n, tags)
      Just (Right n) ->
        (Just n,) <$> getHttps theURI
    theTitle <- asks (flip parseReleaseTitle tags . scraper . snd)
    maybeRelease <- mkRelease lastPath theTitle maybeReleaseNumber theURI finalTags
    maybe
      ($(logError) ("Failed to parse release from " <> render theURI))
      $(logDebugSH)
      maybeRelease
    pure maybeRelease

parseReleaseNumber :: Text -> Maybe (Either Int Int)
parseReleaseNumber lastPath = (Left <$> go "-") <|> (Right <$> go "_")
  where
    go sep = readMaybe (T.unpack (last (T.splitOn sep lastPath)))

newtype ComiCalException = NoParse [Tag ByteString]
  deriving (Show)

instance Exception ComiCalException where
  displayException (NoParse tags) = "Failed to parse tags: " <> show tags

parseReleaseURI :: MonadThrow m => [Tag ByteString] -> m URI
parseReleaseURI tags =
  do
    anchorTag <- headM (NoParse tags) (dropWhile (~/= ("<a>" :: String)) tags)
    mkURI (decodeUtf8 (fromAttrib "href" anchorTag))

mkRelease :: Text -> ByteString -> Maybe Int -> URI -> [Tag ByteString] -> ComiCalApp (Maybe Release)
mkRelease lastPath theTitle maybeReleaseNumber theURI tags =
  asks $
    fmap (Release (BS.pack (T.unpack lastPath)) theTitle maybeReleaseNumber theURI)
      . (flip parseReleaseDate tags . scraper . snd)

throwInvalidUrlM :: MonadThrow m => String -> String -> m a
throwInvalidUrlM url reason = throwM . VanillaHttpException $ InvalidUrlException url reason

headM :: (MonadThrow m, Exception e) => e -> [a] -> m a
headM e xs =
  do
    case listToMaybe xs of
      Nothing -> throwM e
      Just x -> pure x
