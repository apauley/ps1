{-# LANGUAGE OverloadedStrings #-}

module Main where

import Turtle
import Prelude hiding (FilePath)
import GitHellLib
import Data.Text (pack)

main :: IO ()
main = do
  now <- date
  cwd <- pwd
  branch <- currentBranchOrEmptyText
  let prompt = format (s%"\n"%s%" "%fp%"$ ") (showText now) branch (basename cwd)
  echo prompt

showText :: Show a => a -> Text
showText s = pack $ show s
