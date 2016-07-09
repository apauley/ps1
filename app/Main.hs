{-# LANGUAGE OverloadedStrings #-}

module Main where

import Turtle
import Prelude hiding (FilePath)
import HSHLib (maybeFirstLine, terminalColumns)
import GitHellLib (git, currentBranch)
import ANSIColourLib (cyanFG, greenFG, yellowFG, lightRedFG)
import qualified Data.Text as T (justifyRight, pack, unpack, words)
import Data.Maybe
import qualified Control.Foldl as Fold

main :: IO ()
main = do
  now <- date
  cwd <- pwd
  cols <- terminalColumns
  let rightAlignedDate = T.justifyRight (cols-1) '—' $ format (" "%utc) now
  gitLine <- getGitLine
  let prompt = format (s%"\n"%s%"\n"%fp%"$ ") rightAlignedDate gitLine (basename cwd)
  echo prompt

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
