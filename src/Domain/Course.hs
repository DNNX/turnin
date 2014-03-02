module Domain.Course
( Course()
, addGroup
, removeGroup
, getGroups
, getGroup
) where

import Infrastructure.Node
import Domain.Group

data Course = C Node deriving (Show, Eq)

addGroup :: Course -> Group -> Course
addGroup (C node) = C . (`addTo` node)

removeGroup :: Course -> String -> Course
removeGroup (C node) = C . removeChild node

getGroups :: Course -> [String]
getGroups (C node) = map getName $ getChildren node

getGroup :: Course -> String -> Maybe Group
getGroup (C node) = fmap fromNode . getChild node

instance HasNode Course where
 toNode (C n) = wrap n
 fromNode = C
