module Domain.Repo
( Repo()
, addTerm
, removeTerm
, getTerms
, getTerm
) where

import Infrastructure.Node
import Domain.Term

data Repo = R Node deriving (Show, Eq)

addTerm :: Repo -> Term -> Repo
addTerm (R node) = R . (`addTo` node)

removeTerm :: Repo -> String -> Repo
removeTerm (R node) = R . unsetChild node

getTerms :: Repo -> [String]
getTerms (R node) = map getName $ getChildren node

getTerm :: Repo -> String -> Maybe Term
getTerm (R node) = fmap fromNode . getChild node

instance HasNode Repo where
 addTo (R n) p = setChild p n
 fromNode = R