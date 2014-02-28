module Domain.Term
( Term()
, makeTerm
, addCourse
, removeCourse
, getCourses
, getCourse
, getStartDate
, setStartDate
, getEndDate
, setEndDate
, addTermTo
, nodeToTerm
) where

import Infrastructure.Node
import Domain.Course

data Term = T Node deriving (Show, Eq)

makeTerm :: String -> Term
makeTerm = T . makeNode

addCourse :: Term -> Course -> Term
addCourse (T node) = T . (`addCourseTo` node)

removeCourse :: Term -> String -> Term
removeCourse (T node) = T . unsetChild node

getCourses :: Term -> [String]
getCourses (T node) = map getName $ getChildren node

getCourse :: Term -> String -> Maybe Course
getCourse (T node) = fmap nodeToCourse . getChild node

getStartDate :: Term -> String
getStartDate (T node) = getCache node startDate

setStartDate :: Term -> String -> Term
setStartDate (T node) = T . setCache node startDate

getEndDate :: Term -> String
getEndDate (T node) = getCache node endDate

setEndDate :: Term -> String -> Term
setEndDate (T node) = T . setCache node endDate

addTermTo :: Term -> Node -> Node
addTermTo (T node) = flip setChild node

nodeToTerm :: Node -> Term
nodeToTerm = T

startDate = "START_DATE"
endDate = "END_DATE"