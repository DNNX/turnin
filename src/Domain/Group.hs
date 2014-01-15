module Domain.Group
( Group()
, makeGroup
, addProject
, removeProject
, getProjects
, getProject
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