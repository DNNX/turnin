module Interface.CommandLineLexicon where

data Opt = O Char String

-- Commands
configSub = "config"
thresholdSub = "threshold"
termDateSub = "termDate"
setSub = "set"
listSub = "list"

-- Options
helpOpt = O '?' "help"
configThresholdSetCurrentOpt = O 'u' "current"
configThresholdSetChooseOpt = O 'h' "choose"
configTermDateSetTerm1Opt = O '1' "term1"
configTermDateSetTerm2Opt = O '2' "term2"
configTermDateSetTerm3Opt = O '3' "term3"

-- Descriptions
globalDesc = "Global desc"
globalHeader = "Global header"

configDesc = "Config desc"
configThresholdDesc = "Config threshold desc"
configThresholdSetDesc = "Config threshold set desc"
configThresholdListDesc = "Config threshold list desc"
configTermDateDesc = "Config term date desc"
configTermDateSetDesc = "Config term date set desc"
configTermDateListDesc = "Config term date list desc"

-- Metavars
configThresholdSetCurrentMeta = "CURRENT"
configThresholdSetChooseMeta = "CHOOSE"
configTermDateSetTerm1Meta = "TERM1"
configTermDateSetTerm2Meta = "TERM2"
configTermDateSetTerm3Meta = "TERM3"

-- Help messages
helpHelp = "Show this help text"

configThresholdSetCurrentHelp = "Config threshold set current help"
configThresholdSetChooseHelp = "Config threshold set choose help"
configTermDateSetTerm1Help = "Config term date set term1 help"
configTermDateSetTerm2Help = "Config term date set term2 help"
configTermDateSetTerm3Help = "Config term date set term3 help"
                                                         

