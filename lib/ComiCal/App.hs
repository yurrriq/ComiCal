{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TupleSections #-}

-- |
-- Module      : ComiCal.App
-- Copyright   : (c) Eric Bailey, 2019-2023
-- License     : MIT
-- Maintainer  : eric@ericb.me
-- Stability   : experimental
-- Portability : POSIX
module ComiCal.App
  ( ComiCalApp,
    runComiCalApp,
    Publisher (..),
  )
where

import ComiCal.Types (Scraper, Series)
import Control.Monad.Catch (MonadThrow)
import Control.Monad.Logger (LoggingT, MonadLogger, runStderrLoggingT)
import Control.Monad.Reader (MonadIO, MonadReader, ReaderT (..))
import Data.Text (Text)

newtype ComiCalApp a = ComiCalApp
  {runApp :: ReaderT (Text, Publisher) (LoggingT IO) a}
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadLogger,
      MonadIO,
      MonadReader (Text, Publisher),
      MonadFail,
      MonadThrow
    )

runComiCalApp :: (Publisher -> ComiCalApp a) -> Publisher -> Text -> IO a
runComiCalApp getter publisher =
  runStderrLoggingT
    . runReaderT (runApp (getter publisher))
    . (,publisher)

data Publisher = Publisher
  { getCollections :: ComiCalApp Series,
    getIssues :: ComiCalApp Series,
    scraper :: Scraper
  }
