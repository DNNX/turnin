module Domain.Group
( Group()
, makeGroup
, addProject
, removeProject
, getProjects
, getProject
, addTeachers
, removeTeachers
, getTeachers
, addCorrectors
, removeCorrectors
, getCorrectors
) where

import Domain.Project 

data Group = Group deriving (Show, Eq)

makeGroup :: String -> Group
makeGroup = error "Not implemented: Group.makeGroup"

addProject :: Group -> Project -> Group
addProject = error "Not implemented: Group.addProject"

removeProject :: Group -> String -> Group
removeProject = error "Not implemented: Group.removeProject"

getProjects :: Group -> [String]
getProjects = error "Not implemented: Group.getProjects"

getProject :: Group -> String -> Maybe Project
getProject = error "Not implemented: Group.getProject"

addTeachers :: Group -> [String] -> Group
addTeachers = error "Not implemented: Group.addTeachers"

removeTeachers :: Group -> [String] -> Group
removeTeachers = error "Not implemented: Group.removeTeachers"

getTeachers :: Group -> [String]
getTeachers = error "Not implemented: Group.getTeachers"

addCorrectors :: Group -> [String] -> Group
addCorrectors = error "Not implemented: Group.addCorrectors"

removeCorrectors :: Group -> [String] -> Group
removeCorrectors = error "Not implemented: Group.removeCorrectors"

getCorrectors :: Group -> [String]
getCorrectors = error "Not implemented: Group.getCorrectors"
