{-# LANGUAGE OverloadedStrings #-}

module Main where

import Turtle
import Prelude hiding (FilePath)
import Data.Text (pack)

main :: IO ()
main = do
  now <- date
  cwd <- pwd
  echo $ showText now
  echo $ showFilePath cwd

showText :: Show a => a -> Text
showText s = pack $ show s

showFilePath :: FilePath -> Text
showFilePath path = (format fp) path
