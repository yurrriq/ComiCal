{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}

-- |
-- Module      : ComiCal.Types
-- Copyright   : (c) Eric Bailey, 2019-2023
-- License     : MIT
-- Maintainer  : eric@ericb.me
-- Stability   : experimental
-- Portability : POSIX
module ComiCal.Types
  ( Release (Release),
    number,
    Series (Series),
    title,
    releases,
    Event (Event),
    dtstamp,
    uid,
    summary,
    Calendar (Calendar),
    name,
    events,
    HasDate,
    date,
    HasSlug,
    slug,
    HasUri,
    uri,
    Scraper (..),
    PullList (..),
  )
where

import Control.Applicative ((<|>))
import Control.Lens
  ( defaultFieldRules,
    generateUpdateableOptics,
    makeFields,
    makeLensesWith,
    (&),
    (.~),
    (^.),
  )
import Data.Aeson.Types hiding (Series)
import Data.Functor.Syntax ((<$$>))
import qualified Data.List.NonEmpty as NE
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Compat
  ( Day,
    UTCTime (UTCTime),
    addUTCTime,
    defaultTimeLocale,
    formatTime,
    nominalDay,
    secondsToDiffTime,
  )
import GHC.Generics
import Text.Printf (printf)
import Text.URI (URI)
import qualified Text.URI as URI
import Text.XML.Cursor

-- | A 'Release' represents a single issue or a trade paperback.
data Release = Release
  { _releaseSlug :: Text,
    _releaseTitle :: Text,
    _releaseNumber :: Maybe Int,
    _releaseUri :: URI,
    _releaseDate :: Day
  }
  deriving (Eq)

makeLensesWith (defaultFieldRules & generateUpdateableOptics .~ False) ''Release

instance Show Release where
  show release = printf "%s on %s" (T.unpack (release ^. title)) (show (release ^. date))

-- | A 'Series' is a collection of 'Release's.
data Series = Series
  { _seriesTitle :: Text,
    _seriesSlug :: Text,
    _seriesUri :: URI,
    _seriesReleases :: NE.NonEmpty Release
  }
  deriving (Eq)

makeLensesWith (defaultFieldRules & generateUpdateableOptics .~ False) ''Series

instance Show Series where
  show series =
    printf
      "%s (%d releases)"
      (series ^. title)
      (length (series ^. releases))

-- | A calendar 'Event' represents a 'Release' date.
data Event = Event
  { _eventDtstamp :: Day,
    _eventUid :: Text,
    _eventSummary :: Text,
    _eventUri :: URI
  }
  deriving (Eq)

makeLensesWith (defaultFieldRules & generateUpdateableOptics .~ False) ''Event

instance Show Event where
  show event =
    unlines
      [ "BEGIN:VEVENT",
        printf "UID:%s" (event ^. uid),
        printf "SUMMARY:%s" (event ^. summary),
        printf "DTSTART;VALUE=DATE:%s" $ formatDay time,
        printf "DTEND;VALUE=DATE:%s" $ formatDay (addUTCTime nominalDay time),
        printf "URL:%s" (URI.render (event ^. uri)),
        "END:VEVENT"
      ]
    where
      formatDay :: UTCTime -> String
      formatDay = formatTime defaultTimeLocale "%Y%m%d"
      time = UTCTime (event ^. dtstamp) (secondsToDiffTime 0)

-- | A 'Calendar' is a nonempty collection of 'Event's.
data Calendar = Calendar
  { _calendarName :: Text,
    _calendarEvents :: NE.NonEmpty Event
  }
  deriving (Eq)

makeFields ''Calendar

instance Semigroup Calendar where
  a <> b = Calendar ((a ^. name) <> ", " <> (b ^. name)) (a ^. events <> b ^. events)

instance Show Calendar where
  show cal =
    unlines $
      [ "BEGIN:VCALENDAR",
        "PRODID:-//Mozilla.org/NONSGML Mozilla Calendar V1.1//EN",
        "VERSION:2.0",
        printf "X-WR-CALNAME:%s" (cal ^. name)
      ]
        <> NE.toList (show <$> cal ^. events)
        <> ["END:VCALENDAR"]

data Scraper = Scraper
  { partitionReleases :: Cursor -> [Cursor],
    parseTitle :: Cursor -> Text,
    parseReleaseTitle :: Cursor -> Text,
    parseReleaseDate :: Cursor -> Maybe Day
  }

data PullList = PullList
  { dcCollections :: [Text],
    dcIssues :: [Text],
    imageCollections :: [Text],
    imageIssues :: [Text],
    marvelCollections :: [Text],
    marvelIssues :: [Text]
  }
  deriving (Show, Generic)

instance FromJSON PullList where
  parseJSON (Object v) =
    PullList
      <$> maybeSlugs "dcCollections"
      <*> maybeSlugs "dcIssues"
      <*> maybeSlugs "imageCollections"
      <*> maybeSlugs "imageIssues"
      <*> maybeSlugs "marvelCollections"
      <*> maybeSlugs "marvelIssues"
    where
      maybeSlugs key = T.pack <$$> v .: key <|> pure []
  parseJSON invalid =
    prependFailure
      "parsing ComiCalConfig failed, "
      (typeMismatch "Object" invalid)
