module Infrastructure.Node
( Node(Node)
, makeNode
, getName
, getKeys
, getConfig
, setConfig
, unsetConfig
, getCacheKeys
, getCache
, setCache
, unsetCache
, getChildren
, getChild
, setChild
, unsetChild
) where

import qualified Data.Map as M
import Data.Maybe

type Name = String
type Key = [Name]

type Config = M.Map String String
type Cache = M.Map String String
type Children = M.Map String Node

data Node = Node Name Config Cache Children deriving (Show, Eq)

makeNode :: String -> Node
makeNode name = Node name M.empty M.empty M.empty

getName :: Node -> String
getName (Node name _ _ _) = name

getKeys :: Node -> [(Key, Node)]
getKeys n@(Node name _ _ children) = let childrenKeys = concatMap getKeys $ M.elems children
                                         f (k,node) = (name:k,node)
                                     in  ([name],n):map f childrenKeys

getConfig :: Node -> String -> String
getConfig (Node _ config _ _) key = fromMaybe "" $ M.lookup key config

setConfig :: Node -> String -> String -> Node
setConfig node                              key ""     = unsetConfig node key
setConfig (Node name config cache children) key value  = Node name (M.insert key value config) cache children

unsetConfig :: Node -> String -> Node
unsetConfig (Node name config cache children) key = Node name (M.delete key config) cache children

getCacheKeys :: Node -> [String]
getCacheKeys (Node _ _ cache _) = M.keys cache

getCache :: Node -> String -> String
getCache (Node _ _ cache _) key = fromMaybe "" $ M.lookup key cache

setCache :: Node -> String -> String -> Node
setCache node                              key ""    = unsetCache node key
setCache (Node name config cache children) key value = Node name config (M.insert key value cache) children

unsetCache :: Node -> String -> Node
unsetCache (Node name config cache children) key = Node name config (M.delete key cache) children

getChildren :: Node -> [Node]
getChildren (Node _ _ _ children) = M.elems children

getChild :: Node -> String -> Maybe Node
getChild (Node _ _ _ children) key = M.lookup key children

setChild :: Node -> Node -> Node
setChild parent@(Node name config cache children) child@(Node childName _ _ _) =
 case M.lookup childName children of
  Nothing -> Node name config cache $ M.insert childName child children
  Just _  -> parent

unsetChild :: Node -> String -> Node
unsetChild (Node name config cache children) childName = Node name config cache $ M.delete childName children
