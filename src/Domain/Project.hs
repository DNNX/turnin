module Domain.Project
( makeProject
, getStartDate
, setStartDate
, getEndDate
, setEndDate
, getLateDate
, setLateDate
, getNamesToValidate
, addNamesToValidate
, removeNamesToValidate
, getAcceptExecutables
, setAcceptExecutables
, getValidationCommand
, setValidationCommand
, getValidationScript
, setValidationScript
, getTrainScript
, setTrainScript
, getTrainTimeLimit
, setTrainTimeLimit
, getTrainSpaceLimit
, setTrainSpaceLimit
) where

import Infrastructure.Node
import Infrastructure.CsvNode

data Project = P Node ProjectData deriving (Show, Eq)

data ProjectData = D String String deriving (Show, Eq)

makeProject :: String -> Project
makeProject name = P (makeNode name) (D "" "")

getStartDate :: Project -> String
getStartDate (P node _) = getConfig node startDate

setStartDate :: Project -> String -> Project
setStartDate (P node d) newStartDate = P (setConfig node startDate newStartDate) d

getEndDate :: Project -> String
getEndDate (P node _) = getConfig node endDate 

setEndDate :: Project -> String -> Project
setEndDate (P node d) newEndDate = P (setConfig node endDate newEndDate) d

getLateDate :: Project -> String
getLateDate (P node _) = getConfig node lateDate

setLateDate :: Project -> String -> Project
setLateDate (P node d) newLateDate = P (setConfig node lateDate newLateDate) d

getAcceptExecutables :: Project -> String
getAcceptExecutables (P node _) = getConfig node acceptExecutables

setAcceptExecutables :: Project -> String -> Project
setAcceptExecutables (P node d) acceptExec = P (setConfig node acceptExecutables acceptExec) d

getNamesToValidate :: Project -> [String]
getNamesToValidate (P node _) = getCsv node validationNames
 
addNamesToValidate :: Project -> [String] -> Project
addNamesToValidate (P node d) names = P (addCsv node validationNames names) d

removeNamesToValidate :: Project -> [String] -> Project
removeNamesToValidate (P node d) names = P (removeCsv node validationNames names) d

getValidationCommand :: Project -> String
getValidationCommand (P node _) = getConfig node validationCommand

setValidationCommand :: Project -> String -> Project
setValidationCommand (P node d) command = P (setConfig node validationCommand command) d

getValidationScript :: Project -> (String, String)
getValidationScript (P node (D valScript _)) = (getConfig node validationScriptName, valScript) 

setValidationScript :: Project -> String -> String -> Project
setValidationScript (P node (D _ trainScript)) scriptName scriptContent = 
 P (setConfig node validationScriptName scriptName) (D scriptContent trainScript)

getTrainScript :: Project -> (String, String)
getTrainScript (P node (D _ trainScript)) = (getConfig node trainScriptName, trainScript)

setTrainScript :: Project -> String -> String -> Project
setTrainScript (P node (D validationScript _)) scriptName scriptContent = 
 P (setConfig node trainScriptName scriptName) (D validationScript scriptContent)

getTrainTimeLimit :: Project -> String
getTrainTimeLimit (P node _) = getConfig node trainTimeLimit

setTrainTimeLimit :: Project -> String -> Project
setTrainTimeLimit (P node d) timeLimit = P (setConfig node trainTimeLimit timeLimit) d

getTrainSpaceLimit :: Project -> String
getTrainSpaceLimit (P node _) = getConfig node trainSpaceLimit

setTrainSpaceLimit :: Project -> String -> Project
setTrainSpaceLimit (P node d) spaceLimit = P (setConfig node trainSpaceLimit spaceLimit) d

startDate = "START_DATE"
endDate = "END_DATE"
lateDate = "LATE_DATE"
validationNames = "VALIDATION_NAMES"
acceptExecutables = "ACCEPT_EXECUTABLES"
validationCommand = "VALIDATION_COMMAND"
validationScriptName = "VALIDATION_SCRIPT_NAME"
trainScriptName = "TRAIN_SCRIPT_NAME"
trainTimeLimit = "TRAIN_TIME_LIMIT"
trainSpaceLimit = "TRAIN_SPACE_LIMIT"




