{-# LANGUAGE TypeFamilies #-}
module Domain.Group
( Group()
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

instance HasNode Group where
 type ChildType Group = Project 
 toNode (G n) = wrap n; fromNode = G

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

teachers = "TEACHERS"
correctors = "CORRECTORS"
