module Domain.TrainFileRepo
( TrainFileRepo()
, addTrainFile
, removeTrainFile
, getTrainFiles
, getTrainFile
) where

import Infrastructure.Node

data TrainFileRepo = R Node deriving (Show, Eq)

addTrainFile :: TrainFileRepo -> String -> String -> TrainFileRepo
addTrainFile (R node) name = R . setCache node name

removeTrainFile :: TrainFileRepo -> String -> TrainFileRepo
removeTrainFile (R node) = R . flip (setCache node) ""

getTrainFiles :: TrainFileRepo -> [String]
getTrainFiles (R node) = getCacheKeys node

getTrainFile :: TrainFileRepo -> String -> String
getTrainFile (R node) = getCache node

instance HasNode TrainFileRepo where
 addTo (R n) p = setChild p n
 fromNode = R