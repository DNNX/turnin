module Domain.TrainRun
( TrainRun()
, makeTrainRun
, addResult
, removeResult
, getResults
, getResult
, addTrainRunTo
, nodeToTrainRun
) where

import Infrastructure.Node

data TrainRun = T Node deriving (Show, Eq)

makeTrainRun :: String -> TrainRun
makeTrainRun = T . makeNode

addResult :: TrainRun -> String -> String -> TrainRun
addResult (T node) key = T . setCache node key

removeResult :: TrainRun ->  String -> TrainRun
removeResult (T node) = T . flip (setCache node) ""

getResults :: TrainRun ->  [String]
getResults (T node) = getCacheKeys node

getResult :: TrainRun ->  String -> String
getResult (T node) = getCache node

addTrainRunTo :: TrainRun -> Node -> Node
addTrainRunTo (T node) parentNode = setChild parentNode node

nodeToTrainRun :: Node -> TrainRun
nodeToTrainRun = T