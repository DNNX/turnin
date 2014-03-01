module Domain.Course
( Course()
, makeCourse
, addGroup
, removeGroup
, getGroups
, getGroup
) where

import Infrastructure.Node
import Domain.Group

data Course = C Node deriving (Show, Eq)

makeCourse :: String -> Course
makeCourse = C . makeNode

addGroup :: Course -> Group -> Course
addGroup (C node) = C . (`addTo` node)

removeGroup :: Course -> String -> Course
removeGroup (C node) = C . unsetChild node

getGroups :: Course -> [String]
getGroups (C node) = map getName $ getChildren node

getGroup :: Course -> String -> Maybe Group
getGroup (C node) = fmap fromNode . getChild node

instance HasNode Course where
 addTo (C n) p = setChild p n
 fromNode = C