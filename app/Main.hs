{-# LANGUAGE OverloadedStrings #-}

module Main where

import Turtle
import Prelude hiding (FilePath)
import PromptLib (getTimeLine, getGitLine)

main :: IO ()
main = do
  (oneline, trackBranch) <- options "Generates a git-aware shell prompt. export PS1='$(ps1)'" parser

  cwd      <- pwd
  timeLine <- getTimeLine
  gitLine  <- getGitLine $ not oneline
  let prompt = if oneline
        then format (s%" "%fp%"$ ") gitLine (basename cwd)
        else format (s%"\n"%s%fp%"$ ") timeLine gitLine (basename cwd)
  echo prompt

parser :: Parser (Bool, Maybe Text)
parser = (,) <$> switch "oneline" '1' "Don't generate a multi-line prompt"
             <*> optional (optText "track-merge-branch" 't' "Track if you're up to date with a branch that you may need to merge back to, eg. master")
