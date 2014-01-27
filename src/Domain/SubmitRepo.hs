module Domain.SubmitRepo
( SubmitRepo()
, emptySubmitRepo
, addSubmit
, removeSubmit
, getSubmits
, getSubmit
, addLateSubmit
, removeLateSubmit
, getLateSubmits
, addSubmitRepoTo
, nodeToSubmitRepo
, submitRepoName
) where

import Infrastructure.Node
import Infrastructure.CsvNode

data SubmitRepo = R Node deriving (Show, Eq)

emptySubmitRepo :: SubmitRepo
emptySubmitRepo = R $ makeNode submitRepoName 

addSubmit :: SubmitRepo -> String -> String -> SubmitRepo
addSubmit (R node) key = R . setCache node key

removeSubmit :: SubmitRepo -> String -> SubmitRepo
removeSubmit (R node) = R . flip (setCache node) ""

getSubmits :: SubmitRepo -> [String]
getSubmits (R node) = getCacheKeys node

getSubmit :: SubmitRepo -> String -> String
getSubmit (R node) = getCache node

addLateSubmit :: SubmitRepo -> String -> SubmitRepo
addLateSubmit (R node) = R . addCsv node lateSubmit . toList

removeLateSubmit :: SubmitRepo -> String -> SubmitRepo
removeLateSubmit (R node) = R . removeCsv node lateSubmit . toList

getLateSubmits :: SubmitRepo -> [String]
getLateSubmits (R node) = getCsv node lateSubmit

addSubmitRepoTo :: SubmitRepo -> Node -> Node
addSubmitRepoTo (R node) parentNode = setChild parentNode node

nodeToSubmitRepo :: Node -> SubmitRepo
nodeToSubmitRepo = R

toList x = [x]

submitRepoName = "SUBMIT"
lateSubmit = "LATE_SUBMIT"