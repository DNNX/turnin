module Domain.TrainRunRepo
( TrainRunRepo()
, makeTrainRunRepo
, addTrainRun
, removeTrainRun
, getTrainRuns
, getTrainRun
) where

import Infrastructure.Node
import Domain.TrainRun

data TrainRunRepo = R Node deriving (Show, Eq)

makeTrainRunRepo :: String -> TrainRunRepo
makeTrainRunRepo = R . makeNode

addTrainRun :: TrainRunRepo -> TrainRun -> TrainRunRepo
addTrainRun (R node) = R . (`addTrainRunTo` node) 

removeTrainRun :: TrainRunRepo -> String -> TrainRunRepo
removeTrainRun (R node) = R . unsetChild node

getTrainRuns :: TrainRunRepo -> [String]
getTrainRuns (R node) = getChildren node

getTrainRun :: TrainRunRepo -> String -> Maybe TrainRun
getTrainRun (R node) = fmap nodeToTrainRun . getChild node