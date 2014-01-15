module Domain.Root
( Root()
, makeRoot
, addRepo
, removeRepo
, getRepos
, getRepo
) where

import Domain.Repo

data Root = Root deriving (Show, Eq)

makeRoot :: Root
makeRoot = error "Not implemented: Root.makeRoot"

addRepo :: Root -> Repo -> Root
addRepo = error "Not implemented: Root.addRepo"

removeRepo :: Root -> String -> Root
removeRepo = error "Not implemented: Root.removeRepo"

getRepos :: Root -> [String]
getRepos = error "Not implemented: Root.getRepos"

getRepo :: Root -> String -> Maybe Repo
getRepo = error "Not implemented: Root.getRepo"