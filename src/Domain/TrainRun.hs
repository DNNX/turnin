{-# LANGUAGE TypeFamilies #-}
module Domain.TrainRun
( TrainRun()
, addResult
, removeResult
, getResults
, getResult
) where

import Infrastructure.Node

data TrainRun = T Node deriving (Show, Eq)

instance HasNode TrainRun where
 type ChildType TrainRun = ()
 toNode (T n) = wrap n; fromNode = T
 
addResult :: TrainRun -> String -> String -> TrainRun
addResult (T node) key = T . setCache node key

removeResult :: TrainRun ->  String -> TrainRun
removeResult (T node) = T . flip (setCache node) ""

getResults :: TrainRun ->  [String]
getResults (T node) = map fst $ getCachePairs node

getResult :: TrainRun ->  String -> String
getResult (T node) = getCache node

