{-# LANGUAGE OverloadedStrings #-}

module Main where

import Turtle
import Prelude hiding (FilePath)
import HSHLib (maybeFirstLine, terminalColumns)
import GitHellLib (git, currentBranch)
import ANSIColourLib (cyanFG, darkGreyFG, greenFG, yellowFG, lightRedFG)
import qualified Data.Text as T (justifyRight, pack, unpack, words)
import Data.Maybe
import qualified Control.Foldl as Fold

main :: IO ()
main = do
  cwd      <- pwd
  timeLine <- getTimeLine
  gitLine  <- getGitLine
  let prompt = format (s%"\n"%s%"\n"%fp%"$ ") timeLine gitLine (basename cwd)
  echo prompt

getTimeLine :: IO Text
getTimeLine = do
  now  <- date
  cols <- terminalColumns
  let time = T.justifyRight (cols-1) '—' $ format (" "%utc) now
  return $ darkGreyFG time

getGitLine :: IO Text
getGitLine = do
  shortStatus <- maybeFirstLine $ git "status" ["--porcelain"]
  let branchColour = if (shortStatus == Nothing) then greenFG else yellowFG
  branch <- colourOrEmpty branchColour currentBranch
  status <- colourOrEmpty upstreamColour gitStatusUpstream
  return $ format (s%" "%s) branch status

upstreamColour :: Text -> Text
upstreamColour txt = if upToDate then cyanFG txt else lightRedFG txt
  where upToDate = elem "up-to-date" $ T.words txt

gitStatusUpstream :: Shell Text
gitStatusUpstream = do
  let searchText = "Your branch "
  sed (searchText *> return "") $ grep (prefix searchText) (git "status" ["--long"])

colourOrEmpty :: (Text -> Text) -> Shell Text -> IO Text
colourOrEmpty colourFun shellText = do
  line <- maybeFirstLine shellText
  return $ fromMaybe "" $ fmap colourFun line
