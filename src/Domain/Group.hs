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
import Infrastructure.CsvNode
import Domain.Project 

data Group = G Node deriving (Show, Eq)

makeGroup :: String -> Group
makeGroup = G . makeNode

addProject :: Group -> Project -> Group
addProject (G node) = G . (`addProjectTo` node)

removeProject :: Group -> String -> Group
removeProject (G node) = G . unsetChild node

getProjects :: Group -> [String]
getProjects (G node) = map getName $ getChildren node

getProject :: Group -> String -> Maybe Project
getProject (G node) = fmap nodeToProject . getChild node

addTeachers :: Group -> [String] -> Group
addTeachers (G node) = G . addCsv node teachers

removeTeachers :: Group -> [String] -> Group
removeTeachers (G node) = G . removeCsv node teachers

getTeachers :: Group -> [String]
getTeachers (G node) = getCsv node teachers

addCorrectors :: Group -> [String] -> Group
addCorrectors (G node) = G . addCsv node correctors

removeCorrectors :: Group -> [String] -> Group
removeCorrectors (G node) = G . removeCsv node correctors

getCorrectors :: Group -> [String]
getCorrectors (G node) = getCsv node correctors

addGroupTo :: Group -> Node -> Node
addGroupTo (G node) = flip setChild node

nodeToGroup :: Node -> Group
nodeToGroup = G


teachers = "TEACHERS"
correctors = "CORRECTORS"