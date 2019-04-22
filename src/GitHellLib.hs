{-# LANGUAGE OverloadedStrings #-}

module GitHellLib where

import Turtle
import qualified Control.Foldl as Fold
import qualified Data.Text as T (null, strip)

currentBranchOrNothing :: IO (Maybe Text)
currentBranchOrNothing = do
  br <- fmap lineToText currentBranchDiscardErr
  if T.null br then return Nothing else return $ Just br

currentBranchDiscardErr :: IO Line
currentBranchDiscardErr = do
  let shellLine = sed ("* " *> return "") $ grep (prefix "*") (gitDiscardErr "branch" ["--list"])
  fmap unsafeTextToLine (fmap T.strip $ strict shellLine)

emptyError :: Either a Line -> Line
emptyError = either (\a -> "") (\b -> b)

gitDiscardErr :: Text -> [Text] -> Shell Line
gitDiscardErr cmd args = do
  let out = inprocWithErr "git" (cmd:args) empty
  fmap emptyError out
