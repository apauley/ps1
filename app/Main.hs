{-# LANGUAGE OverloadedStrings #-}

module Main where

import Turtle
import Prelude hiding (FilePath)
import Data.Text (pack)

main :: IO ()
main = do
  now <- date
  cwd <- pwd
  let prompt = format (s%"\n"%fp%"$ ") (showText now) (basename cwd)
  echo prompt

showText :: Show a => a -> Text
showText s = pack $ show s
