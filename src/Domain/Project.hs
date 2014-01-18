module Domain.Project
( Project()
, makeProject
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

data Project = P Node deriving (Show, Eq)

makeProject :: String -> Project
makeProject = P . makeNode

getStartDate :: Project -> String
getStartDate (P node) = getConfig node startDate

setStartDate :: Project -> String -> Project
setStartDate (P node) = P . setConfig node startDate  

getEndDate :: Project -> String
getEndDate (P node) = getConfig node endDate 

setEndDate :: Project -> String -> Project
setEndDate (P node) = P . setConfig node endDate

getLateDate :: Project -> String
getLateDate (P node) = getConfig node lateDate

setLateDate :: Project -> String -> Project
setLateDate (P node) = P . setConfig node lateDate

getAcceptExecutables :: Project -> String
getAcceptExecutables (P node) = getConfig node acceptExecutables

setAcceptExecutables :: Project -> String -> Project
setAcceptExecutables (P node) = P . setConfig node acceptExecutables

getNamesToValidate :: Project -> [String]
getNamesToValidate (P node) = getCsv node validationNames
 
addNamesToValidate :: Project -> [String] -> Project
addNamesToValidate (P node) = P . addCsv node validationNames

removeNamesToValidate :: Project -> [String] -> Project
removeNamesToValidate (P node) = P . removeCsv node validationNames

getValidationCommand :: Project -> String
getValidationCommand (P node) = getConfig node validationCommand

setValidationCommand :: Project -> String -> Project
setValidationCommand (P node) = P . setConfig node validationCommand

getValidationScript :: Project -> (String, String)
getValidationScript (P node) = (getConfig node validationScriptName, getCache node validationScriptContent) 

setValidationScript :: Project -> String -> String -> Project
setValidationScript (P node) scriptName = P . setCache (setConfig node validationScriptName scriptName) validationScriptContent

getTrainScript :: Project -> (String, String)
getTrainScript (P node) = (getConfig node trainScriptName, getCache node trainScriptContent)

setTrainScript :: Project -> String -> String -> Project
setTrainScript (P node) scriptName = P . setCache (setConfig node trainScriptName scriptName) trainScriptContent

getTrainTimeLimit :: Project -> String
getTrainTimeLimit (P node) = getConfig node trainTimeLimit

setTrainTimeLimit :: Project -> String -> Project
setTrainTimeLimit (P node) = P . setConfig node trainTimeLimit

getTrainSpaceLimit :: Project -> String
getTrainSpaceLimit (P node) = getConfig node trainSpaceLimit

setTrainSpaceLimit :: Project -> String -> Project
setTrainSpaceLimit (P node) = P . setConfig node trainSpaceLimit

startDate = "START_DATE"
endDate = "END_DATE"
lateDate = "LATE_DATE"
validationNames = "VALIDATION_NAMES"
acceptExecutables = "ACCEPT_EXECUTABLES"
validationCommand = "VALIDATION_COMMAND"
validationScriptName = "VALIDATION_SCRIPT_NAME"
validationScriptContent = "VALIDATION_SCRIPT_NAME"
trainScriptName = "TRAIN_SCRIPT_NAME"
trainScriptContent = "TRAIN_SCRIPT_CONTENT"
trainTimeLimit = "TRAIN_TIME_LIMIT"
trainSpaceLimit = "TRAIN_SPACE_LIMIT"




