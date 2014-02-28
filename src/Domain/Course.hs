module Domain.Course
( Course()
, makeCourse
, addGroup
, removeGroup
, getGroups
, getGroup
, addCourseTo
, nodeToCourse
) where

import Infrastructure.Node
import Domain.Group

data Course = C Node deriving (Show, Eq)

makeCourse :: String -> Course
makeCourse = C . makeNode

addGroup :: Course -> Group -> Course
addGroup (C node) = C . (`addGroupTo` node)

removeGroup :: Course -> String -> Course
removeGroup (C node) = C . unsetChild node

getGroups :: Course -> [String]
getGroups (C node) = map getName $ getChildren node

getGroup :: Course -> String -> Maybe Group
getGroup (C node) = fmap nodeToGroup . getChild node

addCourseTo :: Course -> Node -> Node
addCourseTo (C node) = flip setChild node

nodeToCourse :: Node -> Course
nodeToCourse = C