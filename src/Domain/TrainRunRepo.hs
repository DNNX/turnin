{-# LANGUAGE MultiParamTypeClasses #-}
module Domain.TrainRunRepo
( TrainRunRepo()
) where

import Infrastructure.Node
import Domain.TrainRun

data TrainRunRepo = R Node deriving (Show, Eq)

instance Succ TrainRunRepo TrainRun where
instance HasNode TrainRunRepo where toNode (R n) = wrap n; fromNode = R
