{-# LANGUAGE TypeFamilies #-}
module Domain.TrainRunRepo
( TrainRunRepo()
) where

import Infrastructure.Node
import Domain.TrainRun

data TrainRunRepo = R Node deriving (Show, Eq)

instance HasNode TrainRunRepo where
 type ChildType TrainRunRepo = TrainRun 
 toNode (R n) = wrap n; fromNode = R
