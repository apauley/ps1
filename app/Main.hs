{-# LANGUAGE OverloadedStrings #-}

module Main where

import Turtle
import Prelude hiding (FilePath)
import PromptLib (getTimeLine, getGitLine)

main :: IO ()
main = do
  trackBranch <- options "Generates a git-aware shell prompt. export PS1='$(ps1)'" parser

  cwd      <- pwd
  timeLine <- getTimeLine
  gitLine  <- getGitLine trackBranch
  let prompt = format (s%"\n"%s%fp%"$ ") timeLine gitLine (basename cwd)
  echo prompt

parser :: Parser (Maybe Text)
parser = optional (optText "track-branch" 't'
                   "Track if you're up to date with a branch that you may need to merge back to, eg. origin/master")
