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
type Bag = M.Map String (Either String Node)

data Node = Node Name Config Bag deriving (Show, Eq)

data NodeWrap = W Node
wrap :: Node -> NodeWrap
wrap = W

get hn = let (W n) = toNode hn in n

isRight (Right _) = True
isRight _         = False

fromRight (Right x) = x
fromRight _         = error "fromRight: Left"

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
  make name = Node name M.empty M.empty
  
  addChild parent@(Node name config bag) child =
   let n = get child; childName = getName child
   in  if childName == "" then parent else case M.lookup childName bag of
    Just _  -> parent
    Nothing -> Node name config $ M.insert childName (Right n) bag
  
  removeChild n@(Node name config bag) childName = case M.lookup childName bag of
                                                    Just (Right _) -> Node name config $ M.delete childName bag
                                                    _              -> n
                                                    
  getChildren (Node _ _ bag) = map (fromNode.fromRight) $ filter isRight $ M.elems bag
  getChild (Node _ _ bag) key = f $  M.lookup key bag
   where f (Just (Right x)) = Just $ fromNode x
         f _                = Nothing
  
  getName (Node name _ _) = name
  toNode = W
  fromNode = id

getConfigPairs :: Node -> [(String,String)]
getConfigPairs (Node _ config _) = M.toList config

getConfig :: Node -> String -> String
getConfig (Node _ config _) key = fromMaybe "" $ M.lookup key config

setConfig :: Node -> String -> String -> Node
setConfig node                   "" _       = node
setConfig node                   key ""     = unsetConfig node key
setConfig (Node name config bag) key value  = Node name (M.insert key value config) bag

unsetConfig :: Node -> String -> Node
unsetConfig (Node name config bag) key = Node name (M.delete key config) bag

getCachePairs :: Node -> [(String,String)]
getCachePairs (Node _ _ bag) = map g $ filter f $ M.toList bag
 where f (_,Left _) = True
       f _          = False
       g (x,Left y) = (x,y)
       g _          = error "Node::getCachePairs: error, shouldn't encounter Right value"

getCache :: Node -> String -> String
getCache (Node _ _ bag) key = case M.lookup key bag of
                               Just (Left x) -> x
                               _             -> ""

setCache :: Node -> String -> String -> Node
setCache node                     ""  _     = node
setCache node                     key ""    = unsetCache node key
setCache n@(Node name config bag) key value = case M.lookup key bag of
                                             Just (Right _) -> n
                                             _              -> Node name config $ M.insert key (Left value) bag

unsetCache :: Node -> String -> Node
unsetCache n@(Node name config bag) key = case M.lookup key bag of
                                           Just (Left _) -> Node name config $ M.delete key bag
                                           _             -> n


