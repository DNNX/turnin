module Infrastructure.CsvNode
( getCsv
, setCsv
, addCsv
, removeCsv
) where

import Infrastructure.Node
import Data.List
import Data.List.Split

getCsv :: Node -> String -> [String]
getCsv n key = separate $ getConfig n key

setCsv :: Node -> String -> [String] -> Node
setCsv n key vs = setConfig n key $ unseparate $ sort $ nub vs

addCsv :: Node -> String -> [String] -> Node
addCsv n key vs = setCsv n key $ vs ++ getCsv n key

removeCsv :: Node -> String -> [String] -> Node
removeCsv n key vs = setCsv n key $ getCsv n key \\ vs

separate s = let result = splitOn "," s
             in  if result == [""] then [] else result

unseparate = intercalate ","                                  