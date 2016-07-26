{-# LANGUAGE OverloadedStrings #-}

module Main where

import Turtle
import Prelude hiding (FilePath)
import GitHellLib
import qualified Data.Text as T (pack, unpack, justifyRight)
import Data.Maybe
import qualified Control.Foldl as Fold

main :: IO ()
main = do
  now <- date
  cwd <- pwd
  maybeBranch <- currentBranchOrNothing
  cols <- columns
  let rightAlignedDate = T.justifyRight (cols-1) '—' $ format (" "%utc) now
  let prompt = format (s%"\n"%s%fp%"$ ") rightAlignedDate (branch maybeBranch) (basename cwd)
  echo prompt

columns :: IO Int
columns = do
  let cols = inproc "/usr/bin/env" ["tput", "cols"] empty
  maybeCols <- fold cols Fold.head
  case maybeCols of
    Just c  -> return $ read $ T.unpack c
    Nothing -> return 80

branch :: Maybe Text -> Text
branch maybeBranch = fromMaybe "" $ fmap (format (s%" ")) maybeBranch
