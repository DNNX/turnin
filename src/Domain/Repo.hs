{-# LANGUAGE TypeFamilies #-}
module Domain.Repo
( Repo()
) where

import Infrastructure.Node
import Domain.Term

data Repo = R Node deriving (Show, Eq)

instance HasNode Repo where
 type ChildType Repo = Term
 toNode (R n) = wrap n; fromNode = R

