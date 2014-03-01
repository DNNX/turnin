module Infrastructure.Finder
( findNode
) where

import Infrastructure.Node

findNode :: Node -> [(Maybe String, [Node] -> Maybe String)] -> Maybe Node
findNode = undefined

