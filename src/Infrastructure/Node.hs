module Infrastructure.Node
( Node
, makeNode
, getName
, getKeys
, getConfig
, setConfig
, unsetConfig
, getChild
, setChild
, unsetChild
) where

import qualified Data.Map as M
import Data.Maybe

type Name = String
type Key = [Name]

type Config = M.Map String String
type Children = M.Map String Node

data Node = Node Name Config Children deriving (Show, Eq)

makeNode :: String -> Node
makeNode name = Node name M.empty M.empty

getName :: Node -> String
getName (Node name _ _) = name

getKeys :: Node -> [(Key, Node)]
getKeys n@(Node name _ children) = let childrenKeys = concatMap getKeys $ M.elems children
                                       f (k,node) = (name:k,node)
                                   in  ([name],n):map f childrenKeys

getConfig :: Node -> String -> String 
getConfig (Node _ config _) key = fromMaybe "" $ M.lookup key config

setConfig :: Node -> String -> String -> Node
setConfig (Node name config children) key value = let config' = M.insert key value config
                                                  in  Node name config' children

unsetConfig :: Node -> String -> Node
unsetConfig (Node name config children) key = Node name (M.delete key config) children

getChild :: Node -> String -> Maybe Node
getChild (Node _ _ children) key = M.lookup key children

setChild :: Node -> Node -> Node
setChild parent@(Node name config children) child@(Node childName _ _) =
 case M.lookup childName children of
  Nothing -> Node name config $ M.insert childName child children
  Just _  -> parent

unsetChild :: Node -> String -> Node
unsetChild (Node name config children) childName = Node name config $ M.delete childName children 
