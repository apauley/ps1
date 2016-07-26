{-# LANGUAGE OverloadedStrings #-}

module PromptLib (multiLinePrompt
                 ,singleLinePrompt) where

import Turtle
import Prelude hiding (FilePath)

import HSHLib (maybeFirstLine, terminalColumns)
import GitHellLib (gitDiscardErr, currentBranchDiscardErr)
import ANSIColourLib (brownFG, cyanFG, darkGreyFG, greenFG, lightRedFG, redBG)
import qualified Data.Text as T (justifyRight, null, pack, unpack, words, strip)
import Data.Maybe
import qualified Data.Time.LocalTime as Time
import qualified Data.Time.Format as TF
import qualified Control.Foldl as Fold

multiLinePrompt :: Maybe Text -> IO Text
multiLinePrompt trackBranch = do
  br <- currentBranchDiscardErr
  st <- shortStatus
  timeLine <- getTimeLine
  gitLines <- getGitLines br trackBranch st
  return $ format (s%"\n"%s) timeLine gitLines

singleLinePrompt :: Maybe Text -> IO Text
singleLinePrompt trackBranch = do
  br <- currentBranchDiscardErr
  st <- shortStatus
  return $ colourBranch br st

colourBranch :: Text -> Text -> Text
colourBranch currentBranch shortStatus = do
  let branchColour = if (T.null shortStatus) then greenFG else brownFG
  colourUnlessNull branchColour currentBranch

getTimeLine :: IO Text
getTimeLine = do
  now  <- Time.getZonedTime
  cols <- terminalColumns
  let time = T.pack $ TF.formatTime TF.defaultTimeLocale "%a %b %d, %Y %H:%M:%S" now
  let line = T.justifyRight (cols-1) '—' $ format (" "%s) time
  return $ darkGreyFG line

getGitLines :: Text -> Maybe Text -> Text -> IO Text
getGitLines currentBranch trackBranch shortStatus = do
  let branch = colourBranch currentBranch shortStatus
  status <- colourOrEmpty upstreamColour gitStatusUpstream
  rebase <- fromMaybe (return "") (fmap (rebaseNeeded currentBranch) trackBranch) :: IO Text
  let gitPrompt = format (s%" "%s%" "%s) branch status rebase
  let gp = if (T.null branch)
        then ""
        else gitPrompt
  let lines = if (T.null shortStatus)
        then gp
        else format (s%"\n"%s) gp shortStatus
  return lines

upstreamColour :: Text -> Text
upstreamColour txt = if upToDate then cyanFG txt else lightRedFG txt
  where upToDate = elem "up-to-date" $ T.words txt

shortStatus :: IO Text
shortStatus = strict $ gitDiscardErr "status" ["--short"]

gitStatusUpstream :: Shell Text
gitStatusUpstream = do
  let searchText = "Your branch "
  let st = sed (searchText *> return "") $ grep (prefix searchText) (gitDiscardErr "status" ["--long"])
  sed ((choice [",", ".", "'"]) *> return "") st

colourUnlessNull :: (Text -> Text) -> Text -> Text
colourUnlessNull colourFun txt = if T.null txt then txt else colourFun txt

colourOrEmpty :: (Text -> Text) -> Shell Text -> IO Text
colourOrEmpty colourFun shellText = do
  line <- maybeFirstLine shellText
  return $ fromMaybe "" $ fmap colourFun line

rebaseNeeded :: Text -> Text -> IO Text
rebaseNeeded currentBranch trackBranch = do
  maybeHash <- maybeFirstLine (recentNHashes trackBranch 1) :: IO (Maybe Text)
  let trackedHash = fromMaybe "" maybeHash
  let localHashes = recentNHashes currentBranch 100
  foundHash <- maybeFirstLine $ grep (text trackedHash) localHashes
  return $ fromMaybe (redBG $ format ("Diverged from "%s) trackBranch) $ fmap (\_ -> "") foundHash

recentNHashes :: Text -> Int -> Shell Text
recentNHashes branch limit = gitDiscardErr "log" ["-n", repr limit, "--format=%H", branch]
