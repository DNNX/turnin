module Infrastructure.CsvNode
( getCsv
, setCsv
, addCsv
, removeCsv
) where

import Infrastructure.Node
import qualified Data.Set as S
import Data.List
import Data.List.Split

getCsv :: Node -> String -> [String]
getCsv n key = separate $ getConfig n key

setCsv :: Node -> String -> [String] -> Node
setCsv n key vs = setConfig n key $ unseparate $ sort vs

addCsv :: Node -> String -> String -> Node
addCsv n key v = let vs  = S.fromList $ getCsv n key
                     vs' = S.insert v vs
                 in  setCsv n key $ S.toList vs'

removeCsv :: Node -> String -> String -> Node
removeCsv n key v = let vs  = S.fromList $ getCsv n key
                        vs' = S.delete v vs
                    in  setCsv n key $ S.toList vs'
                    
separate s = let result = splitOn "," s
             in  if result == [""] then [] else result
             
unseparate = intercalate ","                                  