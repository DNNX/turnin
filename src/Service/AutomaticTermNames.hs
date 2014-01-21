module Service.AutomaticTermNames
( make
, isAutoTerm
) where

import Infrastructure.Date as Date
import Data.List.Split
import Data.Maybe

make :: Int -> Date -> [DateDelta] -> [(String, Date, Date)]
make n date ts = let year = getYear date
                     ds = asInfiniteTriples year ts
                     contains (_,d1,d2) = d1 <= date && date <= d2   
                 in  take (fromIntegral n) $ dropWhile (not.contains) ds 
                      
isAutoTerm :: String -> Bool
isAutoTerm s = let parts = splitOn "-" s 
                   maybe_ints = map readPart parts
                   [year, i] = map fromJust maybe_ints
               in  length parts == 2 && all isJust maybe_ints && 1 <= year && year <= 9999 && 1 <= i
                        
-- private utils
asInfiniteTriples year ts = let n = fromIntegral $ length ts
                                xs = zip [1..] $ cycle $ zip [year,year..] ts
                                xs' = zip xs $ tail xs
                                toQuint ((i,(y,t1)),(j,(_,t2))) = (i,j,y,t1,t2)                       
                             in  map (toNDD n.toQuint) xs'
                                 
                
toNDD :: Int -> (Int,Int,Int,DateDelta,DateDelta) -> (String,Date,Date)                                 
toNDD n (i,j,y,t1,t2) = let (q1,r1) = (i-1) `quotRem` n
                            (q2,_) = (j-1) `quotRem` n
                            year1  = y + q1 
                            year2  = y + q2
                            id1    = r1 + 1
                            Right d1 = buildDate year1 t1
                            Right d2 = buildDate year2 t2
                            buildDate y' t = makeDate y' (getDMonth t) (getDDay t) (getDHour t) (getDMinute t)
                            name = Date.pad year1 4 ++ "-" ++ show id1
                        in  (name,d1,d2)
                       
                                               

