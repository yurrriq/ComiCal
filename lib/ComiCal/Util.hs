{-# LANGUAGE TupleSections #-}

-- |
-- Module      : ComiCal.Util
-- Copyright   : (c) Eric Bailey, 2019-2023
-- License     : MIT
-- Maintainer  : eric@ericb.me
-- Stability   : experimental
-- Portability : POSIX
module ComiCal.Util
  ( getHttps,
    parseReleases,
    urlEncode,
  )
where

import ComiCal.App
import ComiCal.Types
import Control.Applicative ((<|>))
import Control.Lens (views, (.~))
import Control.Monad.Catch (Exception, MonadThrow, displayException, throwM)
import Control.Monad.Logger
import Control.Monad.Reader
import qualified Data.List.NonEmpty as NE
import Data.Maybe (catMaybes, listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding as TE
import Network.HTTP.Client (HttpException (..))
import Network.HTTP.Req
import qualified Network.HTTP.Types.URI as HTTP
import Text.HTML.DOM (parseLBS)
import Text.Read (readMaybe)
import Text.URI (URI, mkScheme, mkURI, render, renderStr, unRText)
import Text.URI.Lens (uriPath, uriScheme)
import Text.XML.Cursor

getHttps :: (MonadIO m, MonadLogger m, MonadThrow m) => URI -> m Cursor
getHttps theURI =
  do
    $(logDebug) $ "GET " <> render theURI
    url <- maybe badURI (pure . fst) (useHttpsURI theURI)
    res <- runReq defaultHttpConfig $ req GET url NoReqBody lbsResponse mempty
    pure (fromDocument (parseLBS (responseBody res)))
  where
    badURI = throwInvalidUrlM (renderStr theURI) "Unable to construct Network.HTTP.Req.Url"

parseReleases :: Cursor -> ComiCalApp (NE.NonEmpty Release)
parseReleases cursor =
  asks (partitionReleases . scraper . snd) <*> pure cursor
    >>= fmap (NE.fromList . catMaybes)
      . mapM parseRelease

-- FIXME: This is messy and brittle.
parseRelease :: Cursor -> ComiCalApp (Maybe Release)
parseRelease cursor =
  do
    theURI <- (uriScheme .~ mkScheme "https") <$> parseReleaseURI cursor
    let lastPath = views uriPath (unRText . last) theURI
    (maybeReleaseNumber, finalTags) <- case parseReleaseNumber lastPath of
      Nothing ->
        pure (Nothing, cursor)
      Just (Left n) ->
        pure (Just n, cursor)
      Just (Right n) ->
        (Just n,) <$> getHttps theURI
    theTitle <- asks (flip parseReleaseTitle cursor . scraper . snd)
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

newtype ComiCalException = NoParse Cursor
  deriving (Show)

instance Exception ComiCalException where
  displayException (NoParse tags) = "Failed to parse tags: " <> show tags

parseReleaseURI :: (MonadLogger m, MonadThrow m) => Cursor -> m URI
parseReleaseURI cursor =
  do
    anchorTag <- headM (NoParse cursor) (cursor $// element "a")
    -- FIXME: urlEncode path
    mkURI (head (attribute "href" anchorTag))

mkRelease :: Text -> Text -> Maybe Int -> URI -> Cursor -> ComiCalApp (Maybe Release)
mkRelease lastPath theTitle maybeReleaseNumber theURI tags =
  asks $
    fmap (Release lastPath theTitle maybeReleaseNumber theURI)
      . (flip parseReleaseDate tags . scraper . snd)

throwInvalidUrlM :: MonadThrow m => String -> String -> m a
throwInvalidUrlM url reason = throwM . VanillaHttpException $ InvalidUrlException url reason

headM :: (MonadThrow m, Exception e) => e -> [a] -> m a
headM e xs =
  do
    case listToMaybe xs of
      Nothing -> throwM e
      Just x -> pure x

urlEncode :: Text -> Text
urlEncode = TE.decodeUtf8 . HTTP.urlEncode False . TE.encodeUtf8
