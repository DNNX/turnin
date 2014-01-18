module Domain.Repo
( Repo()
, makeRepo
, addTerm
, removeTerm
, getTerms
, getTerm
, addRepoTo
, nodeToRepo
) where

import Infrastructure.Node
import Domain.Term

data Repo = R Node deriving (Show, Eq)

makeRepo :: String -> Repo
makeRepo = R . makeNode

addTerm :: Repo -> Term -> Repo
addTerm (R node) = R . (`addTermTo` node)

removeTerm :: Repo -> String -> Repo
removeTerm (R node) = R . unsetChild node

getTerms :: Repo -> [String]
getTerms (R node) = getChildren node

getTerm :: Repo -> String -> Maybe Term
getTerm (R node) = fmap nodeToTerm . getChild node 

addRepoTo :: Repo -> Node -> Node
addRepoTo (R node) = flip setChild node

nodeToRepo :: Node -> Repo
nodeToRepo = R