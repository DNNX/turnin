module Infrastructure.Node
( Node
, makeNode
, getName
, getKeys
, getConfig
, setConfig
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
setConfig (Node name config children) key value = let f = if null value then M.delete key else M.insert key value
                                                  in  Node name (f config) children

getChildren :: Node -> [String]
getChildren (Node _ _ children) = M.keys children

getChild :: Node -> String -> Maybe Node
getChild (Node _ _ children) key = M.lookup key children

setChild :: Node -> Node -> Node
setChild parent@(Node name config children) child@(Node childName _ _) =
 case M.lookup childName children of
  Nothing -> Node name config $ M.insert childName child children
  Just _  -> parent

unsetChild :: Node -> String -> Node
unsetChild (Node name config children) childName = Node name config $ M.delete childName children 
