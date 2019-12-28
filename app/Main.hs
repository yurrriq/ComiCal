{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           Data.ComiCal       (_releases, imageComic)
import           Data.Text          (Text, pack)
import           System.Environment (getArgs)


main :: IO ()
main = mapM_ go =<< getSlugs
  where
    go slug = mapM_ print =<< _releases <$> imageComic slug

    getSlugs :: IO [Text]
    getSlugs = getArgs >>= \case
      []    -> error "Must specify slug(s)"
      slugs -> pure (pack <$> slugs)
