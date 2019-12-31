{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS
import           Data.ComiCal          (imageSeries)
import           Data.ComiCal.Types    (Calendar, mkCalendar)
import qualified Data.List.NonEmpty as NE
import           System.Environment    (getArgs)


main :: IO ()
main = print =<< foldr1 (<>) <$> (mapM go =<< getSlugs)
  where
    go :: ByteString -> IO Calendar
    go slug = mkCalendar <$> imageSeries slug

    getSlugs :: IO (NE.NonEmpty ByteString)
    getSlugs = getArgs >>= \case
      []    -> error "Must specify slug(s)"
      slugs -> pure (NE.fromList (BS.pack <$> slugs))
