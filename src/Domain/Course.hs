module Domain.Course
( Course()
, makeCourse
, addGroup
, removeGroup
, getGroups
, getGroup
) where

import Domain.Group

data Course = Course deriving (Show, Eq)

makeCourse :: String -> Course
makeCourse = error "Not implemented: Course.makeCourse"

addGroup :: Course -> Group -> Course
addGroup = error "Not implemented: Course.addGroup"

removeGroup :: Course -> String -> Course
removeGroup = error "Not implemented: Course.removeGroup"

getGroups :: Course -> [String]
getGroups = error "Not implemented: Course.getGroups"

getGroup :: Course -> String -> Maybe Group
getGroup = error "Not implemented: Course.getGroup"