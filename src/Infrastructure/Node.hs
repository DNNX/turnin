{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
module Infrastructure.Node
( Node(Node)
, Succ
, HasNode(make, addTo, addChild, removeChild, getChildren, getChild, getChildrenNames, toNode, fromNode)
, wrap
, getName
, getConfig
, setConfig
, unsetConfig
, getCacheKeys
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

class Succ a b | a -> b where
class HasNode a where
  make :: String -> a
  make = fromNode . make

  addTo :: a -> Node -> Node
  addTo c parentNode = let (W childNode) = toNode c in  addChild parentNode childNode

  addChild :: (Succ a b, HasNode a, HasNode b) => a -> b -> a
  addChild p c = let (W m) = toNode p; (W n) = toNode c; in  fromNode $ addChild m n

  removeChild :: HasNode a => a -> String -> a
  removeChild p = let (W n) = toNode p in  fromNode . removeChild n

  getChildrenNames :: a -> [String]
  getChildrenNames p = let (W n) = toNode p in  getChildrenNames n
  
  getChildren :: (Succ a b, HasNode b) => a -> [b]
  getChildren p = let (W n) = toNode p in  map fromNode $ getChildren n
  
  getChild :: (Succ a b, HasNode b) => a -> String -> Maybe b
  getChild p s = let (W n) = toNode p in  fromNode <$> getChild n s
  
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
  getChildrenNames (Node _ _ _ children) = M.keys children
  getChildren (Node _ _ _ children) = map fromNode $ M.elems children
  getChild (Node _ _ _ children) key = fromNode <$> M.lookup key children
  
  toNode = W
  fromNode = id

getName :: Node -> String
getName (Node name _ _ _) = name

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


