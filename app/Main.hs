{-# LANGUAGE OverloadedStrings #-}

module Main where

import Turtle
import Data.Text (pack)

main :: IO ()
main = do
  now <- date
  echo $ pack $ show now
