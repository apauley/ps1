{-# LANGUAGE OverloadedStrings #-}

module Main where

import Turtle
import Prelude hiding (FilePath)
import PromptLib (getTimeLine, getGitLine)

main :: IO ()
main = do
  trackBranch <- options "Git-aware prompt: https://github.com/apauley/ps1#readme" parser

  timeLine <- getTimeLine
  gitLine  <- getGitLine trackBranch
  let prompt = format (s%"\n"%s) timeLine gitLine
  echo prompt

parser :: Parser (Maybe Text)
parser = subcommand "ml" "Generates a multi-line git-aware shell prompt" mlParser

mlParser :: Parser (Maybe Text)
mlParser = optional (optText "track-branch" 't'
                   "Track if your commits are directly on top of a branch that you may need to merge back to, eg. origin/master")
