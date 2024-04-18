-- |
-- Module      : ComiCal
-- Description : The main API for ComiCal
-- Copyright   : (c) Eric Bailey, 2019-2024
-- License     : MIT
-- Maintainer  : eric@ericb.me
-- Stability   : stable
-- Portability : POSIX
module ComiCal
  ( dcIssues,
    imageCollections,
    imageIssues,
    marvelIssues,
    mkCalendar,
  )
where

import ComiCal.App (Publisher (..), runComiCalApp)
import qualified ComiCal.DC as DC
import qualified ComiCal.Image as Image
import qualified ComiCal.Marvel as Marvel
import ComiCal.Types hiding (PullList (..))
import Control.Lens ((^.))
import qualified Data.List.NonEmpty as NE
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Text.URI (relativeTo)

-- | Given a [DC Comics series](https://www.dccomics.com/comics) slug,
-- fetch, parse, and return the issues of the 'Series'.
dcIssues :: Text -> IO Series
dcIssues = runComiCalApp getIssues DC.publisher

-- | Given an [Image Comics series](https://imagecomics.com/comics/series) slug,
-- fetch, parse, and return the collections of the 'Series'.
imageCollections :: Text -> IO Series
imageCollections = runComiCalApp getCollections Image.publisher

-- | Given an [Image Comics series](https://imagecomics.com/comics/series) slug,
-- fetch, parse, and return the issues of the 'Series'.
imageIssues :: Text -> IO Series
imageIssues = runComiCalApp getIssues Image.publisher

-- | Given a [Marvel Comics series](https://www.marvel.com/comics) slug,
-- fetch, parse, and return the issues of the 'Series'.
marvelIssues :: Text -> IO Series
marvelIssues = runComiCalApp getIssues Marvel.publisher

-- | Given a 'Series', create an 'Event' from each 'Release' and return a
-- 'Calendar'.
mkCalendar :: Series -> Calendar
mkCalendar series = Calendar (series ^. title) $ NE.map go (series ^. releases)
  where
    go :: Release -> Event
    go rel =
      Event (rel ^. date) (rel ^. slug) (rel ^. title) $
        fromMaybe (rel ^. uri) ((rel ^. uri) `relativeTo` (series ^. uri))
