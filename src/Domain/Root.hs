module Domain.Root
( Root()
, makeRoot
, addRepo
, removeRepo
, getRepos
, getRepo
, getCurrentThreshold
, setCurrentThreshold
, getChooseThreshold
, setChooseThreshold
, getTerm1Date
, setTerm1Date
, getTerm2Date
, setTerm2Date
, getTerm3Date
, setTerm3Date
, getProjectEndDate
, setProjectEndDate
, getProjectLateDate
, setProjectLateDate
, getAcceptExecutables
, setAcceptExecutables
, getTrainTimeLimit
, setTrainTimeLimit
, getTrainSpaceLimit
, setTrainSpaceLimit
, getAdminGroups
, setAdminGroups
, getTeacherGroups
, setTeacherGroups
, addCorrector
, removeCorrector
, getCorrectors
) where

import Infrastructure.Node
import Infrastructure.CsvNode
import Domain.Repo
 
data Root = R Node deriving (Show, Eq)

makeRoot :: String -> Root 
makeRoot = R . makeNode

addRepo :: Root -> Repo -> Root
addRepo (R node) = R . (`addRepoTo` node) 

removeRepo :: Root -> String -> Root
removeRepo (R node) = R . unsetChild node

getRepos :: Root -> [String] 
getRepos (R node) = map getName $ getChildren node

getRepo :: Root -> String -> Maybe Repo
getRepo (R node) = fmap nodeToRepo . getChild node 

getCurrentThreshold :: Root -> String
getCurrentThreshold (R node) = getConfig node currentThreshold

setCurrentThreshold :: Root -> String -> Root
setCurrentThreshold (R node) = R . setConfig node currentThreshold

getChooseThreshold :: Root -> String
getChooseThreshold (R node) = getConfig node chooseThreshold

setChooseThreshold :: Root -> String -> Root
setChooseThreshold (R node) = R . setConfig node chooseThreshold

getTerm1Date :: Root -> String
getTerm1Date (R node) = getConfig node term1Date

setTerm1Date :: Root -> String -> Root
setTerm1Date (R node) = R . setConfig node term1Date

getTerm2Date :: Root -> String
getTerm2Date (R node) = getConfig node term2Date

setTerm2Date :: Root -> String -> Root
setTerm2Date (R node) = R . setConfig node term2Date

getTerm3Date :: Root -> String
getTerm3Date (R node) = getConfig node term3Date

setTerm3Date :: Root -> String -> Root
setTerm3Date (R node) = R . setConfig node term3Date

getProjectEndDate :: Root -> String
getProjectEndDate (R node) = getConfig node projectEndDate

setProjectEndDate :: Root -> String -> Root
setProjectEndDate (R node) = R . setConfig node projectEndDate

getProjectLateDate :: Root -> String
getProjectLateDate (R node) = getConfig node projectLateDate

setProjectLateDate :: Root -> String -> Root
setProjectLateDate (R node) = R . setConfig node projectLateDate

getAcceptExecutables :: Root -> String
getAcceptExecutables (R node) = getConfig node acceptExecutables

setAcceptExecutables :: Root -> String -> Root
setAcceptExecutables (R node) = R . setConfig node acceptExecutables

getTrainTimeLimit :: Root -> String
getTrainTimeLimit (R node) = getConfig node trainTimeLimit

setTrainTimeLimit :: Root -> String -> Root
setTrainTimeLimit (R node) = R . setConfig node trainTimeLimit

getTrainSpaceLimit :: Root -> String
getTrainSpaceLimit (R node) = getConfig node trainSpaceLimit

setTrainSpaceLimit :: Root -> String -> Root
setTrainSpaceLimit (R node) = R . setConfig node trainSpaceLimit

getAdminGroups :: Root -> [String]
getAdminGroups (R node) = getCsv node adminGroups

setAdminGroups :: Root -> [String] -> Root
setAdminGroups (R node) = R . setCsv node adminGroups

getTeacherGroups :: Root -> [String]
getTeacherGroups (R node) = getCsv node teacherGroups

setTeacherGroups :: Root -> [String] -> Root
setTeacherGroups (R node) = R . setCsv node teacherGroups

addCorrector :: Root -> String -> Root
addCorrector (R node) = R . addCsv node corrector . (:[])

removeCorrector :: Root -> String -> Root
removeCorrector (R node) = R . removeCsv node corrector . (:[])

getCorrectors :: Root -> [String]
getCorrectors (R node) = getCsv node corrector


currentThreshold = "CURRENT_THRESHOLD"
chooseThreshold = "CHOOSE_THRESHOLD"
term1Date = "TERM1_DATE"
term2Date = "TERM2_DATE"
term3Date = "TERM3_DATE"
projectEndDate = "PROJECT_END_DATE"
projectLateDate = "PROJECT_LATE_DATE"
acceptExecutables = "ACCEPT_EXECUTABLES"
trainTimeLimit = "TRAIN_TIME_LIMIT"
trainSpaceLimit = "TRAIN_SPACE_LIMIT"
adminGroups = "ADMIN_GROUPS"
teacherGroups = "TEACHER_GROUPS"
corrector = "CORRECTOR"

