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
, isCorrector
) where

import Infrastructure.Node
import Domain.Repo
 
data Root = R Node deriving (Show, Eq)

makeRoot :: String -> Root 
makeRoot = R . makeNode

addRepo :: Root -> Repo -> Root
addRepo (R node) = R . (`addRepoTo` node) 

removeRepo :: Root -> String -> Root
removeRepo (R node) = R . unsetChild node

getRepos :: Root -> [String] 
getRepos (R node) = getChildren node

getRepo :: Root -> String -> Maybe Repo
getRepo (R node) = fmap nodeToRepo . getChild node 

getCurrentThreshold :: Root -> String
getCurrentThreshold = error "Not implemented: Root.getCurrentThreshold"

setCurrentThreshold :: Root -> String -> Root
setCurrentThreshold = error "Not implemented: Root.setCurrentThreshold"

getChooseThreshold :: Root -> String
getChooseThreshold = error "Not implemented: Root.getChooseThreshold"

setChooseThreshold :: Root -> String -> Root
setChooseThreshold = error "Not implemented: Root.getChooseThreshold"

getTerm1Date :: Root -> String
getTerm1Date = error "Not implemented: Root.getTerm1Date"

setTerm1Date :: Root -> String -> Root
setTerm1Date = error "Not implemented: Root.setTerm1Date"

getTerm2Date :: Root -> String
getTerm2Date = error "Not implemented: Root.getTerm2Date"

setTerm2Date :: Root -> String -> Root
setTerm2Date = error "Not implemented: Root.setTerm2Date"

getTerm3Date :: Root -> String
getTerm3Date = error "Not implemented: Root.getTerm3Date"

setTerm3Date :: Root -> String -> Root
setTerm3Date = error "Not implemented: Root.setTerm3Date"

getProjectEndDate :: Root -> String
getProjectEndDate = error "Not implemented: Root.getProjectEndDate"

setProjectEndDate :: Root -> String -> Root
setProjectEndDate = error "Not implemented: Root.setProjectEndDate"

getProjectLateDate :: Root -> String
getProjectLateDate = error "Not implemented: Root.getProjectLateDate"

setProjectLateDate :: Root -> String -> Root
setProjectLateDate = error "Not implemented: Root.setProjectLateDate"

getAcceptExecutables :: Root -> String
getAcceptExecutables = error "Not implemented: Root.getAcceptExecutables"

setAcceptExecutables :: Root -> String -> Root
setAcceptExecutables = error "Not implemented: Root.setAcceptExecutables"

getTrainTimeLimit :: Root -> String
getTrainTimeLimit = error "Not implemented: Root.getTrainTimelimit"

setTrainTimeLimit :: Root -> String -> Root
setTrainTimeLimit = error "Not implemented: Root.setTrainTimelimit"

getTrainSpaceLimit :: Root -> String
getTrainSpaceLimit = error "Not implemented: Root.getTrainSpacelimit"

setTrainSpaceLimit :: Root -> String -> Root
setTrainSpaceLimit = error "Not implemented: Root.setTrainSpacelimit"

getAdminGroups :: Root -> [String]
getAdminGroups = error "Not implemented: Root.getAdminGroups"

setAdminGroups :: Root -> [String] -> Root
setAdminGroups = error "Not implemented: Root.setAdminGroups"

getTeacherGroups :: Root -> [String]
getTeacherGroups = error "Not implemented: Root.getTeacherGroups"

setTeacherGroups :: Root -> [String] -> Root
setTeacherGroups = error "Not implemented: Root.setTeacherGroups"

addCorrector :: Root -> String -> Root
addCorrector = error "Not implemented: Root.addCorrector"

removeCorrector :: Root -> String -> Root
removeCorrector = error "Not implemented: Root.removeCorrector"

isCorrector :: Root -> String -> Bool
isCorrector = error "Not implemented: Root.isCorrector"