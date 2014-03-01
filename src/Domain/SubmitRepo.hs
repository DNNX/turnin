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
, submitRepoName
) where

import Infrastructure.Node
import Infrastructure.CsvNode
import Control.Applicative

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
addLateSubmit (R node) = R . addCsv node lateSubmit . pure

removeLateSubmit :: SubmitRepo -> String -> SubmitRepo
removeLateSubmit (R node) = R . removeCsv node lateSubmit . pure

getLateSubmits :: SubmitRepo -> [String]
getLateSubmits (R node) = getCsv node lateSubmit

instance HasNode SubmitRepo where
 addTo (R n) p = setChild p n
 fromNode = R

submitRepoName = "SUBMIT"
lateSubmit = "LATE_SUBMIT"