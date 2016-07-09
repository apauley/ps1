{-# LANGUAGE OverloadedStrings #-}

module Main where

import Turtle
import Prelude hiding (FilePath)
import GitHellLib
import ANSIColourLib
import qualified Data.Text as T (justifyRight, pack, unpack)
import Data.Maybe
import qualified Control.Foldl as Fold

main :: IO ()
main = do
  now <- date
  cwd <- pwd
  cols <- columns
  let rightAlignedDate = T.justifyRight (cols-1) '—' $ format (" "%utc) now
  gitLine <- getGitLine
  let prompt = format (s%"\n"%s%"\n"%fp%"$ ") rightAlignedDate gitLine (basename cwd)
  echo prompt

columns :: IO Int
columns = do
  let cols = inproc "/usr/bin/env" ["tput", "cols"] empty
  maybeCols <- fold cols Fold.head
  case maybeCols of
    Just c  -> return $ read $ T.unpack c
    Nothing -> return 80

getGitLine :: IO Text
getGitLine = do
  branch <- maybeFirstLine currentBranch
  status <- firstLineOrEmpty gitStatusOrigin
  return $ gitLine branch status

gitLine :: Maybe Text -> Text -> Text
gitLine b st = format (s%" "%s) (branch b) (blueFG st)

branch :: Maybe Text -> Text
branch maybeBranch = fromMaybe "" $ fmap greenFG maybeBranch

gitStatusOrigin :: Shell Text
gitStatusOrigin = do
  let searchText = "Your branch "
  sed (searchText *> return "") $ grep (prefix searchText) (git "status" ["--long"])

maybeFirstLine :: Shell Text -> IO (Maybe Text)
maybeFirstLine shellText = fold shellText Fold.head

firstLineOrEmpty :: Shell Text -> IO Text
firstLineOrEmpty shellText = fmap (fromMaybe "") $ maybeFirstLine shellText
