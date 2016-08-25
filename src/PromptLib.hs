{-# LANGUAGE OverloadedStrings #-}

module PromptLib (multiLinePrompt
                 ,singleLinePrompt) where

import Turtle
import Prelude hiding (FilePath)

import HSHLib (maybeFirstLine, terminalColumns)
import GitHellLib (gitDiscardErr, currentBranchDiscardErr)
import ANSIColourLib (ColourFun, brownFG, cyanFG, darkGreyFG, greenFG, lightRedFG, redBG, lightPurpleFG, lightBlueFG)
import qualified Data.Text as T (justifyRight, null, pack, unpack, words, snoc, splitOn, strip)
import Data.Maybe
import qualified Data.Time.LocalTime as Time
import qualified Data.Time.Format as TF
import qualified Control.Foldl as Fold

multiLinePrompt :: IO Text
multiLinePrompt = do
  trackBranch <- readTrackBranch
  br <- currentBranchDiscardErr
  st <- shortStatus
  timeLine <- getTimeLine
  gitLines <- getGitLines br trackBranch st

  hw <- hostPwd
  return $ timeLine <> gitLines <> hw <> "$ "

singleLinePrompt :: IO Text
singleLinePrompt = do
  trackBranch <- readTrackBranch
  br <- currentBranchDiscardErr
  st <- shortStatus

  status   <- strict gitStatusUpstream
  diverged <- fromMaybe (return False) (fmap (trackBranchDiverged br) trackBranch)

  let colour = branchColourShort st status diverged
  let branch = colourUnlessNull colour $ T.strip br
  let gitPrompt = if T.null br then "" else T.snoc branch ' '

  hw <- hostPwd
  return $ gitPrompt <> hw <> "$ "

readTrackBranch :: IO (Maybe Text)
readTrackBranch = do
  let cfgFile = "ps1.cfg"
  cfgExists <- testfile cfgFile
  if cfgExists
    then parseConfig $ input cfgFile
    else return Nothing

parseConfig :: Shell Text -> IO (Maybe Text)
parseConfig shellTxt = do
  txt <- strict $ grep (has "track-branch") shellTxt
  let branchSplit = T.splitOn "=" txt
  case branchSplit of
    (_:branch:_) -> return $ Just (T.strip branch)
    _ -> return Nothing

branchColourShort :: Text -> Text -> Bool -> ColourFun
branchColourShort shortStatus upstreamStatus diverged = if diverged
  then redBG
  else if (T.null shortStatus)
  then upstreamColour upstreamStatus greenFG lightRedFG
  else brownFG

colourBranch :: Text -> Text -> Text
colourBranch currentBranch shortStatus = do
  let branchColour = if (T.null shortStatus) then greenFG else brownFG
  colourUnlessNull branchColour currentBranch

getTimeLine :: IO Text
getTimeLine = do
  now  <- Time.getZonedTime
  cols <- terminalColumns
  let time = T.pack $ TF.formatTime TF.defaultTimeLocale "%a %b %d, %Y %H:%M:%S" now
  let line = T.justifyRight (cols-1) '-' $ format (" "%s) time
  return $ (darkGreyFG line) <> "\n"

getGitLines :: Text -> Maybe Text -> Text -> IO Text
getGitLines currentBranch trackBranch shortStatus = do
  let branch = colourBranch currentBranch shortStatus

  status <- colourOrEmpty upstreamColourSelf gitStatusUpstream
  rebase <- fromMaybe (return "") (fmap (trackBranchText currentBranch) trackBranch) :: IO Text
  let gitPrompt = T.strip $ format (s%" "%s%" "%s) branch status rebase
  let gp = if T.null gitPrompt then "" else T.snoc gitPrompt '\n'

  let lines = if (T.null shortStatus)
        then gp
        else gp <> shortStatus <> "\n"
  return lines

upstreamColourSelf :: ColourFun
upstreamColourSelf st = upstreamColour st cyanFG lightRedFG st

upstreamColour :: Text -> ColourFun -> ColourFun -> ColourFun
upstreamColour st good bad = if upToDate then good else bad
  where upToDate = elem "up-to-date" $ T.words st

shortStatus :: IO Text
shortStatus = fmap T.strip $ strict $ gitDiscardErr "status" ["--short"]

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

trackBranchText :: Text -> Text -> IO Text
trackBranchText currentBranch trackBranch = do
  diverged <- trackBranchDiverged currentBranch trackBranch
  let txt = if diverged
        then format ("Diverged from "%s) trackBranch
        else ""
  return $ colourUnlessNull redBG txt

trackBranchDiverged :: Text -> Text -> IO Bool
trackBranchDiverged currentBranch trackBranch = do
  maybeHash <- maybeFirstLine (recentNHashes trackBranch 1) :: IO (Maybe Text)
  let trackedHash = fromMaybe "" maybeHash
  let localHashes = recentNHashes currentBranch 100
  foundHash <- maybeFirstLine $ grep (text trackedHash) localHashes
  return $ isNothing foundHash

hostPwd :: IO Text
hostPwd = do
  h <- hostname
  w <- promptPwd
  let host = colourUnlessNull lightPurpleFG h
  let cwd  = colourUnlessNull lightBlueFG   w
  return $ host <> ":" <> cwd

promptPwd :: IO Text
promptPwd = do
  h <- home
  let w = inshell "pwd" empty

  let cwd = sed (text (format fp h) *> return "~") w
  fmap T.strip $ strict cwd

recentNHashes :: Text -> Int -> Shell Text
recentNHashes branch limit = gitDiscardErr "log" ["-n", repr limit, "--format=%H", branch]
