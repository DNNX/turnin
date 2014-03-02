module Infrastructure.Node
( Node(Node)
, HasNode(make, addTo, toNode, fromNode)
, wrap
, getName
, getKeys
, getConfig
, setConfig
, unsetConfig
, getCacheKeys
, getCache
, setCache
, unsetCache
, addChild
, removeChild
, getChildren
, getChild
) where

import qualified Data.Map as M
import Control.Applicative
import Data.Maybe

type Name = String
type Key = [Name]

type Config = M.Map String String
type Cache = M.Map String String
type Children = M.Map String Node

data Node = Node Name Config Cache Children deriving (Show, Eq)

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

addChild :: Node -> Node -> Node
addChild parent@(Node name config cache children) child = 
 let (W n) = toNode child
     childName = getName n 
 in  case M.lookup childName children of
  Nothing -> Node name config cache $ M.insert childName n children
  Just _  -> parent

removeChild :: Node -> String -> Node
removeChild (Node name config cache children) childName = Node name config cache $ M.delete childName children

getChildren :: Node -> [Node]
getChildren (Node _ _ _ children) = map fromNode $ M.elems children

getChild :: Node -> String -> Maybe Node
getChild (Node _ _ _ children) key = fromNode <$> M.lookup key children

class HasNode a where
 make :: String -> a
 make = fromNode . make

 addTo :: a -> Node -> Node
 addTo c parentNode = let (W childNode) = toNode c
                      in  fromNode $ addChild parentNode childNode

 toNode :: a -> NodeWrap
 fromNode :: Node -> a
 
instance HasNode Node where
 make name = Node name M.empty M.empty M.empty
 toNode = W
 fromNode = id 

data NodeWrap = W Node

wrap :: Node -> NodeWrap
wrap = W

