{-# LANGUAGE MultiParamTypeClasses #-}
module Domain.Course
( Course()
) where

import Infrastructure.Node
import Domain.Group

data Course = C Node deriving (Show, Eq)

instance Succ Course Group where
instance HasNode Course where toNode (C n) = wrap n; fromNode = C

