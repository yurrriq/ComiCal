{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           Data.ComiCal       (imageComic)
import           Data.Text          (Text, pack)
import           System.Environment (getArgs)


main :: IO ()
main = mapM_ ((print =<<) . imageComic) =<< getSlugs
  where
    getSlugs :: IO [Text]
    getSlugs = getArgs >>= \case
      []    -> error "Must specify slug(s)"
      slugs -> pure (pack <$> slugs)
