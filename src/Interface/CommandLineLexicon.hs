module Interface.CommandLineLexicon where

data Opt = O Char String

currentOpt = O 'u' "current"
chooseOpt = O 'h' "choose"

configSub = "config"

thresholdSub = "threshold"

setSub = "set"