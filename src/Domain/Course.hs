{-# LANGUAGE TypeFamilies #-}
module Domain.Course
( Course()
) where

import Infrastructure.Node
import Domain.Group

data Course = C Node deriving (Show, Eq)

instance HasNode Course where
 type ChildType Course = Group
 toNode (C n) = wrap n; fromNode = C

