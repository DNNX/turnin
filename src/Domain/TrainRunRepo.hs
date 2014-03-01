module Domain.TrainRunRepo
( TrainRunRepo()
, emptyTrainRunRepo
, addTrainRun
, removeTrainRun
, getTrainRuns
, getTrainRun
, trainRunRepoName
) where

import Infrastructure.Node
import Domain.TrainRun

data TrainRunRepo = R Node deriving (Show, Eq)

emptyTrainRunRepo :: TrainRunRepo
emptyTrainRunRepo = R $ makeNode trainRunRepoName

addTrainRun :: TrainRunRepo -> TrainRun -> TrainRunRepo
addTrainRun (R node) = R . (`addTo` node)

removeTrainRun :: TrainRunRepo -> String -> TrainRunRepo
removeTrainRun (R node) = R . unsetChild node

getTrainRuns :: TrainRunRepo -> [String]
getTrainRuns (R node) = map getName $ getChildren node

getTrainRun :: TrainRunRepo -> String -> Maybe TrainRun
getTrainRun (R node) = fmap fromNode . getChild node

trainRunRepoName = "TRAIN_RUN"

instance HasNode TrainRunRepo where
 addTo (R n) p = setChild p n
 fromNode = R