module Domain.Repo
( Repo()
, makeRepo
, addTerm
, removeTerm
, getTerms
, getTerm
) where

import Domain.Term

data Repo = Repo deriving (Show, Eq)

makeRepo :: String -> Repo
makeRepo = error "Not implemented: Repo.makeRepo"

addTerm :: Repo -> Term -> Repo
addTerm = error "Not implemented: Repo.addTerm"

removeTerm :: Repo -> String -> Repo
removeTerm = error "Not implemented: Repo.removeTerm"

getTerms :: Repo -> [String]
getTerms = error "Not implemented: Repo.getTerms"

getTerm :: Repo -> String -> Maybe Term
getTerm = error "Not implemented: Repo.getTerm"