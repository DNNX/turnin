module Domain.Group
( Group()
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

import Infrastructure.Node
import Infrastructure.CsvNode
import Domain.Project

data Group = G Node deriving (Show, Eq)

addProject :: Group -> Project -> Group
addProject (G node) = G . (`addTo` node)

removeProject :: Group -> String -> Group
removeProject (G node) = G . unsetChild node

getProjects :: Group -> [String]
getProjects (G node) = map getName $ getChildren node

getProject :: Group -> String -> Maybe Project
getProject (G node) = fmap fromNode . getChild node

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

instance HasNode Group where
 addTo (G n) p = setChild p n
 fromNode = G

teachers = "TEACHERS"
correctors = "CORRECTORS"