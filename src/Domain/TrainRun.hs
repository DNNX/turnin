module Domain.TrainRun
( TrainRun()
, addResult
, removeResult
, getResults
, getResult
) where

import Infrastructure.Node

data TrainRun = T Node deriving (Show, Eq)

addResult :: TrainRun -> String -> String -> TrainRun
addResult (T node) key = T . setCache node key

removeResult :: TrainRun ->  String -> TrainRun
removeResult (T node) = T . flip (setCache node) ""

getResults :: TrainRun ->  [String]
getResults (T node) = getCacheKeys node

getResult :: TrainRun ->  String -> String
getResult (T node) = getCache node

instance HasNode TrainRun where
 addTo (T n) p = setChild p n
 fromNode = T
