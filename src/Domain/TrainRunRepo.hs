module Domain.TrainRunRepo
( TrainRunRepo()
, emptyTrainRunRepo
, addTrainRun
, removeTrainRun
, getTrainRuns
, getTrainRun
, addTrainRunRepoTo
, nodeToTrainRunRepo
, trainRunRepoName
) where

import Infrastructure.Node
import Domain.TrainRun

data TrainRunRepo = R Node deriving (Show, Eq)

emptyTrainRunRepo :: TrainRunRepo
emptyTrainRunRepo = R $ makeNode trainRunRepoName

addTrainRun :: TrainRunRepo -> TrainRun -> TrainRunRepo
addTrainRun (R node) = R . (`addTrainRunTo` node)

removeTrainRun :: TrainRunRepo -> String -> TrainRunRepo
removeTrainRun (R node) = R . unsetChild node

getTrainRuns :: TrainRunRepo -> [String]
getTrainRuns (R node) = map getName $ getChildren node

getTrainRun :: TrainRunRepo -> String -> Maybe TrainRun
getTrainRun (R node) = fmap nodeToTrainRun . getChild node

addTrainRunRepoTo :: TrainRunRepo -> Node -> Node
addTrainRunRepoTo (R node) parentNode = setChild parentNode node

nodeToTrainRunRepo :: Node -> TrainRunRepo
nodeToTrainRunRepo = R

trainRunRepoName = "TRAIN_RUN"