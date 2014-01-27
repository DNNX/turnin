module Domain.TrainFileRepo
( TrainFileRepo()
, emptyTrainFileRepo
, addTrainFile
, removeTrainFile
, getTrainFiles
, getTrainFile
, addTrainFileRepoTo
, nodeToTrainFileRepo
, trainFileRepoName
) where

import Infrastructure.Node

data TrainFileRepo = R Node deriving (Show, Eq)

emptyTrainFileRepo :: TrainFileRepo
emptyTrainFileRepo = R $ makeNode trainFileRepoName

addTrainFile :: TrainFileRepo -> String -> String -> TrainFileRepo
addTrainFile (R node) name = R . setCache node name

removeTrainFile :: TrainFileRepo -> String -> TrainFileRepo
removeTrainFile (R node) = R . flip (setCache node) ""

getTrainFiles :: TrainFileRepo -> [String]
getTrainFiles (R node) = getCacheKeys node

getTrainFile :: TrainFileRepo -> String -> String
getTrainFile (R node) = getCache node

addTrainFileRepoTo :: TrainFileRepo -> Node -> Node
addTrainFileRepoTo (R node) parentNode = setChild parentNode node

nodeToTrainFileRepo :: Node -> TrainFileRepo
nodeToTrainFileRepo = R

trainFileRepoName = "TRAIN_FILE"