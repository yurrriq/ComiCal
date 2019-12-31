{-# LANGUAGE OverloadedStrings #-}

module Data.ComiCal
  ( imageSeries
  ) where

import           Data.ComiCal.Parser   (parseSeries)
import           Data.ComiCal.Types    (Series)

import           Data.ByteString.Char8 (ByteString)
import           Data.Text             (Text)
import           Network.HTTP.Req      (GET (..), MonadHttp, NoReqBody (..),
                                        bsResponse, defaultHttpConfig, req,
                                        responseBody, runReq, useHttpsURI)
import           Text.HTML.TagSoup     (Tag, parseTags)
import           Text.URI              (URI, mkURI)


-- | Given an [Image Comics series](https://imagecomics.com/comics/series) slug,
-- fetch, parse, and return the 'Series'.
imageSeries :: Text -> IO Series
imageSeries slug = runReq defaultHttpConfig $
    do uri <- mkSeriesListURI slug
       parseSeries slug uri <$> getSeries uri


mkSeriesListURI :: Applicative f => Text -> f URI
mkSeriesListURI slug =
    maybe (error "Failed to construct series URI") pure .
    mkURI $ "https://imagecomics.com/comics/list/series/" <>
    slug <> "/releases"


getSeries :: MonadHttp m => URI -> m [Tag ByteString]
getSeries uri =
    do url <- maybe (error "Failed to GET series") (pure . fst) (useHttpsURI uri)
       parseTags . responseBody <$> req GET url NoReqBody bsResponse mempty
