module Service.WorktrainOutputFormatter
( stripAccents
,formatLineEnds
,wrapLines
,formatRepetitions
,addHeaderAndFooter
,mergeOutputs
,makeRepeatTree
,testValues
) where

import Service.Accents
import Service.AsciiArt
import Data.Maybe

stripAccents :: String -> String
stripAccents = map fromAccent

formatLineEnds :: String -> String 
formatLineEnds [] = []
formatLineEnds ('\r':s) = formatLineEnds s
formatLineEnds (c:s) = c:formatLineEnds s

wrapLines :: String -> Int -> String
wrapLines s n = let ls = lines s
                    ls' = concatMap (wrap n) ls
                in  unlines ls'

wrap _ [] = []
wrap n s = let (begin,rest) = splitAt n s
           in  begin:wrap n rest

addHeaderAndFooter :: String -> String -> String
addHeaderAndFooter toFormat key = unlines [toAsciiArt key,key,begin,toFormat,end]
 where begin = " ============ output begin ============ "
       end   = " ============  output end  ============ " 

mergeOutputs :: [String] -> String
mergeOutputs = unlines


-- Repetitions
formatRepetitions :: String -> Int -> String
formatRepetitions s n = let ls = lines s
                            t = makeRepeatTree $ map L ls
                            t' = truncateRepeatTree n t
                        in  unlines $ reverse $ treeToLines t'

truncateRepeatTree :: Int -> Tree -> Tree
truncateRepeatTree _ x@(L _)   = x
truncateRepeatTree n (N nb xs) = let xs' = map (truncateRepeatTree n) xs
                                     nb' = min nb n
                                     nbLines = length $ concatMap treeToLines xs'
                                     nbTimes = nb - n
                                     msg = "[The previous " ++ plural nbLines "line" ++ " repeat " ++ plural nbTimes "time" ++ "]"
                                 in  if nb > n then N 1 [L msg,N nb' xs'] else N nb' xs'

plural i s = show i ++ " " ++ s ++ (if i >= 2 then "s" else "")

treeToLines :: Tree -> [String]
treeToLines (L s) = [s]
treeToLines (N nb ts) = let ls = concatMap treeToLines ts
                       in  concat $ replicate nb ls

data Tree = L String | N Int [Tree] deriving (Show,Eq)

makeRepeatTree ts' = f ts'[]
 where f []     [t]   = t
       f []     ts    = N 1 ts
       f (t:ts) stack = f ts $ reduce (t:stack) 1 1

reduce :: [Tree] -> Int -> Int -> [Tree]
reduce ts n m = let (first,second,rest) = pop ts n m
                    (first',second',merged) = merge first second
                    ts' = first' ++ second' ++ rest
                in  if merged 
                    then reduce ts' 1 1
                    else if null second
                         then ts'
                         else if m < n
                              then reduce ts n (m+1)
                              else reduce ts (n+1) 1

pop ts n m = let (first,ts') = splitAt n ts
                 (second,rest) = splitAt m ts'
             in  (first,second,rest) 

merge :: [Tree] -> [Tree] -> ([Tree],[Tree],Bool)
merge [] [] = ([],[],False)
merge [] ys = ([],ys,False)
merge xs [] = (xs,[],False)
merge xs ys = let mx = maxLevel xs
                  my = maxLevel ys
                  xs' = map (toLevel mx) xs
                  ys' = map (toLevel my) ys
                  ex = getEffectiveLevel xs'
                  ey = getEffectiveLevel ys'
                  n = max ex ey
                  x = extractSingleTree n ex mx xs'
                  y = extractSingleTree n ey my ys'
                  result = merge' x y
              in  if ex > ey || ey - ex > 1 || isNothing result then (xs,ys,False) else ([],[fromJust result],True)

getEffectiveLevel :: [Tree] -> Int
getEffectiveLevel xs = let n = maxLevel xs
                       in  n + if length xs == 1 then 0 else 1              
              
maxLevel :: [Tree] -> Int
maxLevel = maximum . map getLevel     

getLevel :: Tree -> Int
getLevel (L _) = 0
getLevel (N _ xs) = 1 + maxLevel xs      

toLevel :: Int -> Tree -> Tree
toLevel n x = f (getLevel x) x
 where f m y | m == n    = y
             | otherwise = f (m+1) (N 1 [y])
       
extractSingleTree :: Int -> Int -> Int -> [Tree] -> Tree       
extractSingleTree n _ m [t] | n == m     = t
extractSingleTree n _ m ts  | n == m + 1 = N 1 ts
extractSingleTree n e m ts  | n > e      = extractSingleTree n e (m+1) [N 1 ts]
extractSingleTree n e m ts               = error $ "Cannot extract single tree from n: " ++ show n ++ ",e: " ++ show e ++ ",m: " ++ show m ++ ",ts: " ++ show ts
       
merge' :: Tree -> Tree -> Maybe Tree       
merge' (L x) (L y)       | x == y   = Just $ N 2 [L x]
merge' (N i xs) (N j ys) | xs == ys = Just $ N (i+j) xs
merge' _        _                   = Nothing

testValues = [ (l0,l0_expected)
             , (l1,l1_expected)
             , (l2,l2_expected)
             , (l3,l3_expected)
             , (t0,t0_expected)
             , (t1,t1_expected)
             , (t2,t2_expected)
             , (t3,t3_expected)
             , (t4,t4_expected)
             ]

l0 = [a,a,a]
l0_expected = N 3 [a]

l1 = [a,b,a,b,a,b]
l1_expected = N 3 [b,a]

l2 = [a,b,a,b,a,b,c,a,b,a,b,a,b,c,a,b,a,b,a,b,c]
l2_expected = N 3 [N 1 [c],N 3 [b,a]]

l3 = [a,a,b,b,c,c,  a,a,b,b,a,a,b,b,a,a,b,b,c,  a,a,a,b,b,b,a,a,a,b,b,b,c,c,  a,a,a,b,b,b,a,a,a,b,b,b,c,c,  a,a,a,b,b,b,a,a,a,b,b,b,c,c]
l3_expected = N 1 [N 3 [N 1 [N 2 [c]],N 2 [N 3 [b],N 3 [a]]],c,N 3 [N 2 [b],N 2 [a]],N 2 [c],N 2 [b],N 2 [a]]

t0 = replicate 10 a
t0_expected = N 10 [a]

t1 = [a,b,a,b,a,b,a,b,a,b,a,b,a,b,a,b,a,b,a,b]
t1_expected = N 10 [b,a]

t2 = [a,b,a,b,a,b,a,b,a,b,c,
      a,b,a,b,a,b,a,b,a,b,c,
      a,b,a,b,a,b,a,b,a,b,c,
      a,b,a,b,a,b,a,b,a,b,c,
      a,b,a,b,a,b,a,b,a,b,c,
      a,b,a,b,a,b,a,b,a,b,c,
      a,b,a,b,a,b,a,b,a,b,c,
      a,b,a,b,a,b,a,b,a,b,c,
      a,b,a,b,a,b,a,b,a,b,c,
      a,b,a,b,a,b,a,b,a,b,c]
t2_expected = N 10 [N 1 [c],N 5 [b,a]]

t3 = replicate 10 a ++ replicate 10 b ++ replicate 10 c ++ [d] ++
      concat (replicate 10 (replicate 10 a ++ replicate 10 b)) ++ [c] ++ 
       concat (replicate 10 (replicate 10 a ++ replicate 10 b ++ replicate 10 c))
t3_expected = N 1 [N 10 [N 10 [c],N 10 [b],N 10 [a]],c,N 10 [N 10 [b],N 10 [a]],d,N 10 [c],N 10 [b],N 10 [a]]

t4 = [ a,b,b,c,c,c,c,c,c,c,c
     , a,b,b,c,c,c,c,c,c,c,c
     , a,b,b,c,c,c,c,c,c,c,c
     , a,b,b,c,c,c,c,c,c,c,c
     , a,b,b,c,c,c,c,c,c,c,c
     , a,b,b,c,c,c,c,c,c,c,c
     , a,b,b,c,c,c,c,c,c,c,c
     ]
     
t4_expected =  N 7 [N 8 [c],N 2 [b],N 1 [a]]
   
a = L "a"
b = L "b"
c = L "c"
d = L "d"
              
                    