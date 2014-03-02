{-# LANGUAGE MultiParamTypeClasses #-}
module Domain.Repo
( Repo()
) where

import Infrastructure.Node
import Domain.Term

data Repo = R Node deriving (Show, Eq)

instance Succ Repo Term where
instance HasNode Repo where toNode (R n) = wrap n; fromNode = R

