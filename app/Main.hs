{-# LANGUAGE OverloadedStrings #-}

module Main where

import Turtle
import Prelude hiding (FilePath)
import GitHellLib
import qualified Data.Text as T (pack, unpack)
import qualified Control.Foldl as Fold

main :: IO ()
main = do
  now <- date
  cwd <- pwd
  maybeBranch <- currentBranchOrNothing
  cols <- columns
  echo $ showText cols
  let prompt = format (s%"\n"%s%fp%"$ ") (showText now) (branch maybeBranch) (basename cwd)
  echo prompt

columns :: IO Int
columns = do
  let cols = inproc "/usr/bin/env" ["tput", "cols"] empty
  maybeCols <- fold cols Fold.head
  case maybeCols of
    Just c  -> return $ read $ T.unpack c
    Nothing -> return 80

branch :: Maybe Text -> Text
branch maybeBranch = case maybeBranch of
  Just b  -> format (s%" ") b
  Nothing -> ""

showText :: Show a => a -> Text
showText s = T.pack $ show s
