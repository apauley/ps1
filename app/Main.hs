{-# LANGUAGE OverloadedStrings #-}

module Main where

import Turtle
import Prelude hiding (FilePath)
import PromptLib (multiLinePrompt, colouredBranch)
import HSHLib (noArgs)

data PromptCommand = MultiLine (Maybe Text) | Branch (Maybe Text) deriving (Show)

main :: IO ()
main = do
  x <- options "Git-aware prompt: https://github.com/apauley/ps1#readme" parser
  case x of
    MultiLine trackBranch -> multiLinePrompt trackBranch
    Branch _ -> colouredBranch >>= echo

parser :: Parser PromptCommand
parser = fmap MultiLine (subcommand "ml" "Generates a multi-line git-aware shell prompt" mlParser)
     <|> fmap Branch (subcommand "br" "Returns a coloured branch name" noArgs)

mlParser :: Parser (Maybe Text)
mlParser = optional (optText "track-branch" 't'
                   "Track if your commits are directly on top of a branch that you may need to merge back to, eg. origin/master")
