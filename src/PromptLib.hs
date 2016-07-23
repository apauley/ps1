{-# LANGUAGE OverloadedStrings #-}

module PromptLib where

import Turtle
import Prelude hiding (FilePath)

import HSHLib (maybeFirstLine, terminalColumns)
import GitHellLib (gitDiscardErr, currentBranchDiscardErr)
import ANSIColourLib (cyanFG, darkGreyFG, greenFG, yellowFG, lightRedFG)
import qualified Data.Text as T (justifyRight, null, pack, unpack, words)
import Data.Maybe
import qualified Data.Time.LocalTime as Time
import qualified Data.Time.Format as TF
import qualified Control.Foldl as Fold

getTimeLine :: IO Text
getTimeLine = do
  now  <- Time.getZonedTime
  cols <- terminalColumns
  let time = T.pack $ TF.formatTime TF.defaultTimeLocale "%Y-%m-%d %H:%M:%S" now
  let line = T.justifyRight (cols-1) '—' $ format (" "%s) time
  return $ darkGreyFG line

getGitLine :: Maybe Text -> Bool -> IO Text
getGitLine trackBranch multiline = do
  shortStatus <- maybeFirstLine $ gitDiscardErr "status" ["--short"]
  let modified = fromMaybe "" $ fmap (format ("\n"%s)) shortStatus
  let branchColour = if (shortStatus == Nothing) then greenFG else yellowFG
  branch <- colourOrEmpty branchColour currentBranchDiscardErr
  status <- colourOrEmpty upstreamColour gitStatusUpstream
  rebase <- fromMaybe (return "") (fmap rebaseNeeded trackBranch) :: IO Text
  let gitPrompt = if multiline
        then format (s%" "%s%" "%s%s%"\n") branch status rebase modified
        else format (s%" "%s) branch status
  let lines = if (T.null branch)
        then ""
        else gitPrompt
  return lines

upstreamColour :: Text -> Text
upstreamColour txt = if upToDate then cyanFG txt else lightRedFG txt
  where upToDate = elem "up-to-date" $ T.words txt

gitStatusUpstream :: Shell Text
gitStatusUpstream = do
  let searchText = "Your branch "
  let st = sed (searchText *> return "") $ grep (prefix searchText) (gitDiscardErr "status" ["--long"])
  sed ((choice [",", ".", "'"]) *> return "") st

colourOrEmpty :: (Text -> Text) -> Shell Text -> IO Text
colourOrEmpty colourFun shellText = do
  line <- maybeFirstLine shellText
  return $ fromMaybe "" $ fmap colourFun line

rebaseNeeded :: Text -> IO Text
rebaseNeeded trackBranch = do
  maybeHash <- maybeFirstLine (recentNHashes trackBranch 1) :: IO (Maybe Text)
  let trackedHash = fromMaybe "" maybeHash
  let localHashes = recentNHashes "master" 100
  foundHash <- maybeFirstLine $ grep (text trackedHash) localHashes
  return $ fromMaybe "Needs rebase" $ fmap (\_ -> "Up to date") foundHash

recentNHashes :: Text -> Int -> Shell Text
recentNHashes branch limit = gitDiscardErr "log" ["-n", repr limit, "--format=%H", branch]
