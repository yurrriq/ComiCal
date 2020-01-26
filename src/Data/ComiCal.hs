{-# LANGUAGE OverloadedStrings #-}

module Data.ComiCal
  ( imageCollections, imageIssues
  ) where

import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS
import           Data.ComiCal.Parser   (parseSeries)
import           Data.ComiCal.Types    (Series)
import qualified Data.Text             as Text
import           Network.HTTP.Req      (GET (..), MonadHttp, NoReqBody (..),
                                        bsResponse, defaultHttpConfig, req,
                                        responseBody, runReq, useHttpsURI)
import           Text.HTML.TagSoup     (Tag, parseTags)
import           Text.URI              (URI, mkURI)


-- | Given an [Image Comics series](https://imagecomics.com/comics/series) slug,
-- fetch, parse, and return the collections of the 'Series'.
imageCollections :: ByteString -> IO Series
imageCollections slug = runReq defaultHttpConfig $
    do uri <- mkSeriesCollectionsURI slug
       parseSeries slug uri <$> getSeries uri


-- | Given an [Image Comics series](https://imagecomics.com/comics/series) slug,
-- fetch, parse, and return the issues of the 'Series'.
imageIssues :: ByteString -> IO Series
imageIssues slug = runReq defaultHttpConfig $
    do uri <- mkSeriesListURI slug
       parseSeries slug uri <$> getSeries uri


mkSeriesListURI :: Applicative f => ByteString-> f URI
mkSeriesListURI slug =
    maybe (error "Failed to construct series URI") pure .
    mkURI $ "https://imagecomics.com/comics/list/series/" <>
    (Text.pack (BS.unpack slug)) <> "/releases"


mkSeriesCollectionsURI :: Applicative f => ByteString-> f URI
mkSeriesCollectionsURI slug =
  maybe (error "Failed to construct series URI") pure .
    mkURI $ "https://imagecomics.com/comics/list/series/" <>
    (Text.pack (BS.unpack slug)) <> "/collected-editions"


getSeries :: MonadHttp m => URI -> m [Tag ByteString]
getSeries uri =
    do url <- maybe (error "Failed to GET issues") (pure . fst) (useHttpsURI uri)
       parseTags . responseBody <$> req GET url NoReqBody bsResponse mempty
