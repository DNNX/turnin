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
, addGroupTo
, nodeToGroup
) where

import Infrastructure.Node
import Domain.Project 

data Group = G Node deriving (Show, Eq)

makeGroup :: String -> Group
makeGroup = G . makeNode

addProject :: Group -> Project -> Group
addProject (G node) = G . (`addProjectTo` node)

removeProject :: Group -> String -> Group
removeProject (G node) = G . unsetChild node

getProjects :: Group -> [String]
getProjects (G node) = getChildren node

getProject :: Group -> String -> Maybe Project
getProject (G node) = fmap nodeToProject . getChild node

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

addGroupTo :: Group -> Node -> Node
addGroupTo (G node) = flip setChild node

nodeToGroup :: Node -> Group
nodeToGroup = G