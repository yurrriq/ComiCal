{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           Data.ComiCal (imageComic)


main :: IO ()
main = print =<< imageComic "gideon-falls"
