module Domain.TrainRunRepo
( TrainRunRepo()
, addTrainRun
, removeTrainRun
, getTrainRuns
, getTrainRun
) where

import Infrastructure.Node
import Domain.TrainRun

data TrainRunRepo = R Node deriving (Show, Eq)

addTrainRun :: TrainRunRepo -> TrainRun -> TrainRunRepo
addTrainRun (R node) = R . (`addTo` node)

removeTrainRun :: TrainRunRepo -> String -> TrainRunRepo
removeTrainRun (R node) = R . removeChild node

getTrainRuns :: TrainRunRepo -> [String]
getTrainRuns (R node) = map getName $ getChildren node

getTrainRun :: TrainRunRepo -> String -> Maybe TrainRun
getTrainRun (R node) = fmap fromNode . getChild node

instance HasNode TrainRunRepo where
 toNode (R n) = wrap n
 fromNode = R
