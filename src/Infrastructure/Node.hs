{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
module Infrastructure.Node
( Node(Node)
, Succ
, HasNode(make, getName, addTo, addChild, removeChild, getChildren, getChild, toNode, fromNode)
, wrap
, getConfigPairs
, getConfig
, setConfig
, unsetConfig
, getCachePairs
, getCache
, setCache
, unsetCache
) where

import qualified Data.Map as M
import Control.Applicative
import Data.Maybe

type Name = String

type Config = M.Map String String
type Cache = M.Map String String
type Children = M.Map String Node

data Node = Node Name Config Cache Children deriving (Show, Eq)

data NodeWrap = W Node
wrap :: Node -> NodeWrap
wrap = W

get hn = let (W n) = toNode hn in n

class Succ a b | a -> b where
class HasNode a where
  make :: String -> a
  make = fromNode . make
  
  addTo :: HasNode b => Node -> a -> b
  addTo p = fromNode . addChild p . get

  getName :: a -> String
  getName =  getName . get
  
  addChild :: (Succ a b, HasNode a, HasNode b) => a -> b -> a
  addChild p c = fromNode $ addChild (get p) (get c)

  removeChild :: HasNode a => a -> String -> a
  removeChild p = fromNode . removeChild (get p)

  getChildren :: (Succ a b, HasNode b) => a -> [b]
  getChildren = map fromNode . getChildren . get
  
  getChild :: (Succ a b, HasNode b) => a -> String -> Maybe b
  getChild p s = fromNode <$> getChild (get p) s
  
  toNode :: a -> NodeWrap
  fromNode :: Node -> a

instance Succ Node Node where
instance HasNode Node where
  make name = Node name M.empty M.empty M.empty
  
  addChild parent@(Node name config cache children) child =
   let (W n) = toNode child; childName = getName n
   in  case M.lookup childName children of
    Nothing -> Node name config cache $ M.insert childName n children
    Just _  -> parent
  
  removeChild (Node name config cache children) childName = Node name config cache $ M.delete childName children
  getChildren (Node _ _ _ children) = map fromNode $ M.elems children
  getChild (Node _ _ _ children) key = fromNode <$> M.lookup key children
  
  getName (Node name _ _ _) = name
  toNode = W
  fromNode = id

getConfigPairs :: Node -> [(String,String)]
getConfigPairs (Node _ config _ _) = M.toList config

getConfig :: Node -> String -> String
getConfig (Node _ config _ _) key = fromMaybe "" $ M.lookup key config

setConfig :: Node -> String -> String -> Node
setConfig node                              key ""     = unsetConfig node key
setConfig (Node name config cache children) key value  = Node name (M.insert key value config) cache children

unsetConfig :: Node -> String -> Node
unsetConfig (Node name config cache children) key = Node name (M.delete key config) cache children

getCachePairs :: Node -> [(String,String)]
getCachePairs (Node _ _ cache _) = M.toList cache

getCache :: Node -> String -> String
getCache (Node _ _ cache _) key = fromMaybe "" $ M.lookup key cache

setCache :: Node -> String -> String -> Node
setCache node                              key ""    = unsetCache node key
setCache (Node name config cache children) key value = Node name config (M.insert key value cache) children

unsetCache :: Node -> String -> Node
unsetCache (Node name config cache children) key = Node name config (M.delete key cache) children


