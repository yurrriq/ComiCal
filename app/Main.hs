{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           Control.Lens       (view)
import           Data.ComiCal       (imageSeries)
import           Data.ComiCal.Types (releases)
import           Data.Text          (Text, pack)
import           System.Environment (getArgs)


main :: IO ()
main = mapM_ go =<< getSlugs
  where
    go :: Text -> IO ()
    go slug = mapM_ print =<< view releases <$> imageSeries slug

    getSlugs :: IO [Text]
    getSlugs = getArgs >>= \case
      []    -> error "Must specify slug(s)"
      slugs -> pure (pack <$> slugs)
