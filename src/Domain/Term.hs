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
) where

import Domain.Course

data Term = Term deriving (Show, Eq)

makeTerm :: String -> Term
makeTerm = error "Not implemented: Term.makeTerm"

addCourse :: Term -> Course -> Term
addCourse = error "Not implemented: Term.addCourse"

removeCourse :: Term -> String -> Term
removeCourse = error "Not implemented: Term.removeCourse"

getCourses :: Term -> [String]
getCourses = error "Not implemented: Term.getCourses"

getCourse :: Term -> String -> Maybe Course
getCourse = error "Not implemented: Term.getCourse"

getStartDate :: Term -> String
getStartDate = error "Not implemented: Term.getStartDate"

setStartDate :: Term -> String -> Term
setStartDate = error "Not implemented: Term.setStartDate"

getEndDate :: Term -> String
getEndDate = error "Not implemented: Term.getEndDate"

setEndDate :: Term -> String -> Term
setEndDate = error "Not implemented: Term.setEndDate"