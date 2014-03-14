module TestUtils where

import Data.List
import Infrastructure.Node
import Control.Arrow (second)

uniqueNonEmpty = nub . filter (not.null)
uniqueNonEmptyNoComma = uniqueNonEmpty . map (filter (/=','))

applyGets x = nub . map ($ x)

applySets x = nub . map (foldl apply x) . permutations
 where apply y (g,d) = g y d

areEqual xs = [head xs] == nub xs
sameElements xs ys = null(xs \\ ys) && null (ys \\ xs)

fromTrip f (a,b,c) = f a b c

clampS :: Int -> Int -> Int -> Int
clampS minV maxV v = let [x,y] = sort [minV,maxV] in f x y $ abs v
 where f mi ma val
        | mi == ma  = mi
        | otherwise = ((val-mi) `mod` (ma-mi+1)) + mi

clampL :: Int -> Int -> [a] -> [a]
clampL minL maxL l = let len = clampS minL maxL $ length l
                     in  take len l

unclamp :: Int -> Int -> Int -> Int
unclamp minV maxV v = let [x,y] = sort [minV,maxV] in f x y v
 where f mi ma val
        | mi == ma = ma + 1
        | mi <= val && val <= ma = if val `mod` 2 == 0 then mi - 1 else ma + 1
        | otherwise              = val

fromRight (Right x) = x
fromRight _         = error "Should be right value"

fromLeft (Left x) = x
fromLeft _        = error "Should be left value"

isLeft = not . isRight

isRight (Right _) = True
isRight _         = False

increment pos = f pos []
 where f 0 front (x:back) = reverse front ++ [x+1] ++ back
       f i front (x:back) = f (i-1) (x:front) back
       f _ front []       = reverse front

makeNode :: String -> Int -> Int -> [String] -> Node
makeNode rootName nbChildren maxDepth nodeNames = let (Just result,_) = f nbChildren maxDepth (rootName:nodeNames) in result
 where f _ _ []     = (Nothing,[])
       f _ 0 (x:xs) = (Just $ make x, xs)
       f n d (x:xs) = let (result,rest) = g n xs $ make x
                          g 0  ns m = (m,ns)
                          g nb ns m = let g' = g (nb-1) ns'; (c,ns') = f (nb-1) (d-1) ns in g' $ maybe m (addChild m) c
                      in  (Just result,rest)

makeKeys :: Node -> [(Node,[String])]
makeKeys = map (second reverse) . f []
 where f parentKey n = let key = getName n:parentKey
                           p = (n,key)
                           ps = concatMap (f key) $ getChildren n
                       in  (p:ps)

         