{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.ComiCal.Types
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
    mkCalendar,
    HasDate,
    date,
    HasSlug,
    slug,
    HasUri,
    uri,
  )
where

import Control.Lens
  ( defaultFieldRules,
    generateUpdateableOptics,
    makeFields,
    makeLensesWith,
    (&),
    (.~),
    (^.),
  )
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS
import qualified Data.List.NonEmpty as NE
import Data.Maybe (fromMaybe)
import Data.Time.Compat
  ( Day,
    UTCTime (UTCTime),
    addUTCTime,
    defaultTimeLocale,
    formatTime,
    nominalDay,
    secondsToDiffTime,
  )
import Text.Printf (printf)
import Text.URI (URI, relativeTo)
import qualified Text.URI as URI

-- | A 'Release' has an 'issue' number, a 'uri', and a 'date'.
data Release = Release
  { _releaseSlug :: ByteString,
    _releaseTitle :: ByteString,
    _releaseNumber :: Maybe Int,
    _releaseUri :: URI,
    _releaseDate :: Day
  }
  deriving (Eq)

makeLensesWith (defaultFieldRules & generateUpdateableOptics .~ False) ''Release

instance Show Release where
  show release = printf "%s on %s" (BS.unpack (release ^. title)) (show (release ^. date))

-- | A 'Series' has a 'title', a 'uri', and a list of 'releases'.
data Series = Series
  { _seriesTitle :: ByteString,
    _seriesSlug :: ByteString,
    _seriesUri :: URI,
    _seriesReleases :: NE.NonEmpty Release
  }
  deriving (Eq)

makeLensesWith (defaultFieldRules & generateUpdateableOptics .~ False) ''Series

instance Show Series where
  show series =
    printf
      "%s (%d releases)"
      (BS.unpack (series ^. title))
      (length (series ^. releases))

data Event = Event
  { _eventDtstamp :: Day,
    _eventUid :: ByteString,
    _eventSummary :: ByteString,
    _eventUri :: URI
  }
  deriving (Eq)

makeLensesWith (defaultFieldRules & generateUpdateableOptics .~ False) ''Event

instance Show Event where
  show event =
    unlines
      [ "BEGIN:VEVENT",
        printf "UID:%s" (BS.unpack (event ^. uid)),
        printf "SUMMARY:%s" (BS.unpack (event ^. summary)),
        printf "DTSTART;VALUE=DATE:%s" $ formatDay time,
        printf "DTEND;VALUE=DATE:%s" $ formatDay (addUTCTime nominalDay time),
        printf "URL:%s" (URI.render (event ^. uri)),
        "END:VEVENT"
      ]
    where
      formatDay = formatTime defaultTimeLocale "%Y%m%d"
      time = UTCTime (event ^. dtstamp) (secondsToDiffTime 0)

data Calendar = Calendar
  { _calendarName :: ByteString,
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
        printf "X-WR-CALNAME:%s" (BS.unpack (cal ^. name))
      ]
        <> NE.toList (show <$> cal ^. events)
        <> ["END:VCALENDAR"]

mkCalendar :: Series -> Calendar
mkCalendar series = Calendar (series ^. title) $ NE.map go (series ^. releases)
  where
    go :: Release -> Event
    go rel =
      Event (rel ^. date) (rel ^. slug) (rel ^. title) $
        fromMaybe (rel ^. uri) ((rel ^. uri) `relativeTo` (series ^. uri))
