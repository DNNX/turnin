module Domain.SubmitRepo
( SubmitRepo()
, addSubmit
, removeSubmit
, getSubmits
, getSubmit
, addLateSubmit
, removeLateSubmit
, getLateSubmits
) where

import Infrastructure.Node
import Infrastructure.CsvNode
import Control.Applicative

data SubmitRepo = R Node deriving (Show, Eq)

instance HasNode SubmitRepo where toNode (R n) = wrap n; fromNode = R

addSubmit :: SubmitRepo -> String -> String -> SubmitRepo
addSubmit (R node) key = R . setCache node key

removeSubmit :: SubmitRepo -> String -> SubmitRepo
removeSubmit (R node) = R . flip (setCache node) ""

getSubmits :: SubmitRepo -> [String]
getSubmits (R node) = map fst $ getCachePairs node

getSubmit :: SubmitRepo -> String -> String
getSubmit (R node) = getCache node

addLateSubmit :: SubmitRepo -> String -> SubmitRepo
addLateSubmit (R node) = R . addCsv node lateSubmit . pure

removeLateSubmit :: SubmitRepo -> String -> SubmitRepo
removeLateSubmit (R node) = R . removeCsv node lateSubmit . pure

getLateSubmits :: SubmitRepo -> [String]
getLateSubmits (R node) = getCsv node lateSubmit
 
lateSubmit = "LATE_SUBMIT"
