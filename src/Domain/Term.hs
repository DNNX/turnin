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
getCourses (T node) = getChildren node

getCourse :: Term -> String -> Maybe Course
getCourse (T node) = fmap nodeToCourse . getChild node

getStartDate :: Term -> String
getStartDate = error "Not implemented: Term.getStartDate"

setStartDate :: Term -> String -> Term
setStartDate = error "Not implemented: Term.setStartDate"

getEndDate :: Term -> String
getEndDate = error "Not implemented: Term.getEndDate"

setEndDate :: Term -> String -> Term
setEndDate = error "Not implemented: Term.setEndDate"

addTermTo :: Term -> Node -> Node
addTermTo (T node) = flip setChild node

nodeToTerm :: Node -> Term
nodeToTerm = T