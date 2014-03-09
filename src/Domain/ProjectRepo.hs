{-# LANGUAGE TypeFamilies #-}
module Domain.ProjectRepo
( ProjectRepo ()
, makeProjectSubmitRepo
, makeProjectTrainFileRepo
, makeProjectTrainRunRepo
) where
import Infrastructure.Node

import Domain.SubmitRepo
import Domain.TrainFileRepo
import Domain.TrainRunRepo
import Domain.TrainRun

data ProjectRepo = S SubmitRepo | TF TrainFileRepo | TR TrainRunRepo deriving (Show,Eq)

instance HasNode ProjectRepo where
 type ChildType ProjectRepo = TrainRun
 
 getName (S r) = getName r
 getName (TF r) = getName r
 getName (TR r) = getName r
 
 getChildren (S _) = []
 getChildren (TF _) = []
 getChildren (TR r) = getChildren r
 
 getChild (S _) _ = Nothing
 getChild (TF _) _ = Nothing
 getChild (TR r) s = getChild r s

 toNode (S r) = toNode r
 toNode (TF r) = toNode r
 toNode (TR r) = toNode r
 
 fromNode = TR . fromNode
 
makeProjectSubmitRepo = S
makeProjectTrainFileRepo = TF
makeProjectTrainRunRepo = TR