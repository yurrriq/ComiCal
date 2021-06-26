{-# LANGUAGE TupleSections #-}

module ComiCal
  ( dcIssues,
    imageCollections,
    imageIssues,
    mkCalendar,
  )
where

import ComiCal.App (runApp)
import qualified ComiCal.DC as DC
import qualified ComiCal.Image as Image
import ComiCal.Types
import Control.Lens ((^.))
import Control.Monad.Reader (runReaderT)
import Data.ByteString.Char8 (ByteString)
import qualified Data.List.NonEmpty as NE
import Data.Maybe (fromMaybe)
import Text.URI (relativeTo)

-- | Given a [DC Comics series](https://www.dccomics.com/comics) slug,
-- fetch, parse, and return the issues of the 'Series'.
dcIssues :: ByteString -> IO Series
dcIssues = runReaderT (runApp DC.getIssues) . (,DC.config)

-- | Given an [Image Comics series](https://imagecomics.com/comics/series) slug,
-- fetch, parse, and return the collections of the 'Series'.
imageCollections :: ByteString -> IO Series
imageCollections = runReaderT (runApp Image.getCollections) . (,Image.config)

-- | Given an [Image Comics series](https://imagecomics.com/comics/series) slug,
-- fetch, parse, and return the issues of the 'Series'.
imageIssues :: ByteString -> IO Series
imageIssues = runReaderT (runApp Image.getIssues) . (,Image.config)

mkCalendar :: Series -> Calendar
mkCalendar series = Calendar (series ^. title) $ NE.map go (series ^. releases)
  where
    go :: Release -> Event
    go rel =
      Event (rel ^. date) (rel ^. slug) (rel ^. title) $
        fromMaybe (rel ^. uri) ((rel ^. uri) `relativeTo` (series ^. uri))
