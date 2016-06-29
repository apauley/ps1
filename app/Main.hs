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
  maybeBranch <- currentBranchOrNothing
  let prompt = format (s%"\n"%s%""%fp%"$ ") (showText now) (branch maybeBranch) (basename cwd)
  echo prompt

branch :: Maybe Text -> Text
branch maybeBranch = case maybeBranch of
  Just b  -> format (s%" ") b
  Nothing -> ""

showText :: Show a => a -> Text
showText s = pack $ show s
