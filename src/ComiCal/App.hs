{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module ComiCal.App where

import Control.Monad.Catch
import Control.Monad.Reader
import Data.ByteString.Char8 (ByteString)
import Data.Time.Compat
import Text.HTML.TagSoup

newtype ComiCalApp a = ComiCalApp
  {runApp :: ReaderT (ByteString, ComiCalConfig) IO a}
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadReader (ByteString, ComiCalConfig),
      MonadFail,
      MonadThrow
    )

data ComiCalConfig = ComiCalConfig
  { partitionReleases :: [Tag ByteString] -> [[Tag ByteString]],
    parseTitle :: [Tag ByteString] -> ByteString,
    parseReleaseTitle :: [Tag ByteString] -> ByteString,
    parseReleaseDate :: [Tag ByteString] -> Maybe Day
  }
