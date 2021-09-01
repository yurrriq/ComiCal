{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TupleSections #-}

-- |
-- Module      : ComiCal.App
-- Copyright   : (c) Eric Bailey, 2019-2021
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

import ComiCal.Types
import Control.Monad.Catch
import Control.Monad.Reader
import Data.ByteString.Char8 (ByteString)

newtype ComiCalApp a = ComiCalApp
  {runApp :: ReaderT (ByteString, Publisher) IO a}
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadReader (ByteString, Publisher),
      MonadFail,
      MonadThrow
    )

runComiCalApp :: (Publisher -> ComiCalApp a) -> Publisher -> ByteString -> IO a
runComiCalApp getter publisher = runReaderT (runApp (getter publisher)) . (,publisher)

data Publisher = Publisher
  { getCollections :: ComiCalApp Series,
    getIssues :: ComiCalApp Series,
    scraper :: Scraper
  }
