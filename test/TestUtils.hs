module TestUtils where

import Data.List

uniqueNonEmpty = nub . filter (not.null)
uniqueNonEmptyNoComma = uniqueNonEmpty . map (filter (/=','))

applyGets x = nub . map ($ x)

applySets x = nub . map (foldl apply x) . permutations
 where apply y (g,d) = g y d
 
areEqual xs = [head xs] == nub xs
sameElements xs ys = null(xs \\ ys) && null (ys \\ xs)

fromTrip f (a,b,c) = f a b c

clamp :: Integer -> Integer -> Integer -> Integer
clamp v minV maxV = let [x,y] = sort [minV,maxV] in f x y $ abs v
 where f mi ma val
        | mi == ma  = mi
        | otherwise = val `mod` (ma-mi+1) + mi
        
unclamp :: Integer -> Integer -> Integer -> Integer
unclamp v minV maxV = let [x,y] = sort [minV,maxV] in f x y v
 where f mi ma val
        | mi == ma = ma + 1
        | mi <= val && val <= ma = if val `mod` 2 == 0 then mi - 1 else ma + 1
        | otherwise               = val     

pad :: Integer -> Integer -> String
pad v l = let s = show v
              s' = replicate (fromIntegral l-length s) '0'
          in  s' ++ s

fromRight (Right x) = x
fromRight _         = error "Should be right value"

fromLeft (Left x) = x
fromLeft _        = error "Should be left value"

isRight (Right _) = True
isRight _         = False

increment pos = f pos []
 where f 0 front (x:back) = reverse front ++ [x+1] ++ back
       f i front (x:back) = f (i-1) (x:front) back
       f _ front []       = reverse front
