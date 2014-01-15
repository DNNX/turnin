module Domain.Term
( Term()
, makeTerm
, addCourse
, removeCourse
, getCourses
, getCourse
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