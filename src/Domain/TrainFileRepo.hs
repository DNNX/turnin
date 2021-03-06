{-# LANGUAGE TypeFamilies #-}
module Domain.TrainFileRepo
( TrainFileRepo()
, addTrainFile
, removeTrainFile
, getTrainFiles
, getTrainFile
) where

import Infrastructure.Node

data TrainFileRepo = R Node deriving (Show, Eq)

instance HasNode TrainFileRepo where
 type ChildType TrainFileRepo = ()
 toNode (R n) = wrap n; fromNode = R

addTrainFile :: TrainFileRepo -> String -> String -> TrainFileRepo
addTrainFile (R node) name = R . setCache node name

removeTrainFile :: TrainFileRepo -> String -> TrainFileRepo
removeTrainFile (R node) = R . flip (setCache node) ""

getTrainFiles :: TrainFileRepo -> [String]
getTrainFiles (R node) = map fst $ getCachePairs node

getTrainFile :: TrainFileRepo -> String -> String
getTrainFile (R node) = getCache node

