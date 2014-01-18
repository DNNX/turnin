module Domain.TrainFileRepo
( TrainFileRepo()
, makeTrainFileRepo
, addTrainFile
, removeTrainFile
, getTrainFiles
, getTrainFile
) where

import Infrastructure.Node

data TrainFileRepo = R Node deriving (Show, Eq)

makeTrainFileRepo :: String -> TrainFileRepo
makeTrainFileRepo = R . makeNode

addTrainFile :: TrainFileRepo -> String -> String -> TrainFileRepo
addTrainFile (R node) name = R . setCache node name

removeTrainFile :: TrainFileRepo -> String -> TrainFileRepo
removeTrainFile (R node) = R . flip (setCache node) ""

getTrainFiles :: TrainFileRepo -> [String]
getTrainFiles (R node) = getCacheKeys node

getTrainFile :: TrainFileRepo -> String -> String
getTrainFile (R node) = getCache node