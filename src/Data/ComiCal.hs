{-# LANGUAGE TupleSections #-}

module Data.ComiCal
  ( imageCollections,
    imageIssues,
    dcIssues,
  )
where

import Control.Monad.Reader (runReaderT)
import Data.ByteString.Char8 (ByteString)
import Data.ComiCal.App (runApp)
import qualified Data.ComiCal.DC as DC
import qualified Data.ComiCal.Image as Image
import Data.ComiCal.Types (Series)

-- | Given an [Image Comics series](https://imagecomics.com/comics/series) slug,
-- fetch, parse, and return the collections of the 'Series'.
imageCollections :: ByteString -> IO Series
imageCollections = runReaderT (runApp Image.getCollections) . (,Image.config)

-- | Given an [Image Comics series](https://imagecomics.com/comics/series) slug,
-- fetch, parse, and return the issues of the 'Series'.
imageIssues :: ByteString -> IO Series
imageIssues = runReaderT (runApp Image.getIssues) . (,Image.config)

-- | Given a [DC Comics series](https://www.dccomics.com/comics) slug,
-- fetch, parse, and return the issues of the 'Series'.
dcIssues :: ByteString -> IO Series
dcIssues = runReaderT (runApp DC.getIssues) . (,DC.config)
