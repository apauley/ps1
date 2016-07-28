{-# LANGUAGE OverloadedStrings #-}

module ANSIColourLib where

import Turtle

type ColourFun = Text -> Text
type ColourCode = Text

-- \x01 corresponds to '\[' in Bash
-- \x02 corresponds to '\]' in Bash
-- http://www.tldp.org/HOWTO/Bash-Prompt-HOWTO/x329.html
-- https://superuser.com/questions/301353/escape-non-printing-characters-in-a-function-for-a-bash-prompt/301355
colourText :: ColourCode -> ColourFun
colourText colourCode = format ("\x01\x1b["%s%"m\x02"%s%"\x01\x1b[0m\x02") colourCode

blackFG       = colourText codeBlackFG
darkGreyFG    = colourText codeDarkGreyFG

redFG         = colourText codeRedFG
lightRedFG    = colourText codeLightRedFG

greenFG       = colourText codeGreenFG
lightGreenFG  = colourText codeLightGreenFG

brownFG       = colourText codeBrownFG
yellowFG      = colourText codeYellowFG

blueFG        = colourText codeBlueFG
lightBlueFG   = colourText codeLightBlueFG

purpleFG      = colourText codePurpleFG
lightPurpleFG = colourText codeLightPurpleFG

cyanFG        = colourText codeCyanFG
lightCyanFG   = colourText codeLightCyanFG

lightGrayFG   = colourText codeLightCyanFG
whiteFG       = colourText codeWhiteFG

redBG         = colourText codeRedBG

blueOnRed     = blueFG . redBG

codeBlackFG       = "0;30"
codeDarkGreyFG    = "1;30"

codeRedFG         = "0;31"
codeLightRedFG    = "1;31"

codeGreenFG       = "0;32"
codeLightGreenFG  = "1;32"

codeBrownFG       = "0;33"
codeYellowFG      = "1;33"

codeBlueFG        = "0;34"
codeLightBlueFG   = "1;34"

codePurpleFG      = "0;35"
codeLightPurpleFG = "1;35"

codeCyanFG        = "0;36"
codeLightCyanFG   = "1;36"

codeLightGrayFG   = "0;37"
codeWhiteFG       = "1;37"

codeRedBG         = "0;41"
