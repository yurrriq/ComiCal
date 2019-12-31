{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TemplateHaskell        #-}

module Data.ComiCal.Types
  ( Release (..), issue
  , Series (..), title, releases
  , HasDate, date
  , HasUri, uri
  ) where

import           Control.Lens          (defaultFieldRules,
                                        generateUpdateableOptics,
                                        makeLensesWith, (&), (.~), (^.))
import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS
import           Data.Time             (Day)
import           Text.Printf           (printf)
import           Text.URI              (URI)


-- | A 'Release' has an 'issue' number, a 'uri', and a 'date'.
data Release = Release
  { _releaseIssue :: Int
  , _releaseUri   :: URI
  , _releaseDate  :: Day }
  deriving (Eq)

makeLensesWith (defaultFieldRules & generateUpdateableOptics .~ False) ''Release

instance Show Release where
    show release =
      printf "#%d on %s" (release^.issue) (show (release^.date))


-- | A 'Series' has a 'title', a 'uri', and a list of 'releases'.
data Series = Series
  { _seriesTitle    :: ByteString
  , _seriesUri      :: URI
  , _seriesReleases :: [Release] }
  deriving (Eq)

makeLensesWith (defaultFieldRules & generateUpdateableOptics .~ False) ''Series

instance Show Series where
  show series = printf "%s (%d releases)"
                (BS.unpack (series^.title))
                (length (series^.releases))
