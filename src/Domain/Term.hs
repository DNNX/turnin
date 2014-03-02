module Domain.Term
( Term()
, addCourse
, removeCourse
, getCourses
, getCourse
, getStartDate
, setStartDate
, getEndDate
, setEndDate
) where

import Infrastructure.Node
import Domain.Course

data Term = T Node deriving (Show, Eq)

addCourse :: Term -> Course -> Term
addCourse (T node) = T . (`addTo` node)

removeCourse :: Term -> String -> Term
removeCourse (T node) = T . unsetChild node

getCourses :: Term -> [String]
getCourses (T node) = map getName $ getChildren node

getCourse :: Term -> String -> Maybe Course
getCourse (T node) = fmap fromNode . getChild node

getStartDate :: Term -> String
getStartDate (T node) = getCache node startDate

setStartDate :: Term -> String -> Term
setStartDate (T node) = T . setCache node startDate

getEndDate :: Term -> String
getEndDate (T node) = getCache node endDate

setEndDate :: Term -> String -> Term
setEndDate (T node) = T . setCache node endDate

instance HasNode Term where
 toNode (T n) = wrap n
 fromNode = T

startDate = "START_DATE"
endDate = "END_DATE"
