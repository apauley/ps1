{-# LANGUAGE OverloadedStrings #-}

module Main where

import Turtle
import Prelude hiding (FilePath)
import PromptLib (multiLinePrompt, singleLinePrompt)
import HSHLib (noArgs)

data PromptCommand = MultiLine (Maybe Text) | OneLine (Maybe Text) deriving (Show)

main :: IO ()
main = do
  x <- options "Git-aware prompt: https://github.com/apauley/ps1#readme" parser
  case x of
    MultiLine trackBranch -> multiLinePrompt  >>= echo
    OneLine   trackBranch -> singleLinePrompt >>= echo

parser :: Parser PromptCommand
parser = fmap MultiLine (subcommand "ml" "Generates a multi-line git-aware shell prompt"  noArgs)
     <|> fmap OneLine   (subcommand "sl" "Generates a single-line git-aware shell prompt" noArgs)
