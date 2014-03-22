{-# OPTIONS_GHC -F -pgmF htfpp -fno-warn-incomplete-patterns#-}
{-# LANGUAGE FlexibleInstances #-}
module Infrastructure.Finder.FinderPropTest where

import Test.Framework
import Control.Monad.State
import Data.List hiding (find)
import Control.Arrow (second)

import Infrastructure.Finder.FinderTestUtils
import Infrastructure.Node

prop_noFindFromRoot rootName ns s = let ns' = filter (not.null) $ nub (s:ns) in length ns' > 1 ==> let (suffix:names) = ns' in
  let root = makeNode rootName 4 7 names
      keys = makeKeys root
  in all (uncurry (presentOnFind root)) keys
  && all (uncurry (presentOnFindAll root)) keys
  && all (absentByName root suffix.snd) keys
  && all (absentByNoChild root.snd) keys

presentOnFind :: Node -> Node -> [String] -> Bool
presentOnFind rt n [a]               = a == getName n && assertStateP (zero                findP rt ldN                             0) 1 [(zeroK,n)]                && assertStateP (zero                findUnambiguousP rt ldN                             0) 1 (Just n)
presentOnFind rt n [a,b]             = b == getName n && assertStateP (one   b             findP rt ldN ldN                         0) 2 [(oneK a,n)]               && assertStateP (one   b             findUnambiguousP rt ldN ldN                         0) 2 (Just n)
presentOnFind rt n [a,b,c]           = c == getName n && assertStateP (two   b c           findP rt ldN ldN ldN                     0) 3 [(twoK a b,n)]             && assertStateP (two   b c           findUnambiguousP rt ldN ldN ldN                     0) 3 (Just n)
presentOnFind rt n [a,b,c,d]         = d == getName n && assertStateP (three b c d         findP rt ldN ldN ldN ldN                 0) 4 [(threeK a b c,n)]         && assertStateP (three b c d         findUnambiguousP rt ldN ldN ldN ldN                 0) 4 (Just n)
presentOnFind rt n [a,b,c,d,e]       = e == getName n && assertStateP (four  b c d e       findP rt ldN ldN ldN ldN ldN             0) 5 [(fourK a b c d,n)]        && assertStateP (four  b c d e       findUnambiguousP rt ldN ldN ldN ldN ldN             0) 5 (Just n)
presentOnFind rt n [a,b,c,d,e,f]     = f == getName n && assertStateP (five  b c d e f     findP rt ldN ldN ldN ldN ldN ldN         0) 6 [(fiveK a b c d e,n)]      && assertStateP (five  b c d e f     findUnambiguousP rt ldN ldN ldN ldN ldN ldN         0) 6 (Just n)
presentOnFind rt n [a,b,c,d,e,f,g]   = g == getName n && assertStateP (six   b c d e f g   findP rt ldN ldN ldN ldN ldN ldN ldN     0) 7 [(sixK a b c d e f,n)]     && assertStateP (six   b c d e f g   findUnambiguousP rt ldN ldN ldN ldN ldN ldN ldN     0) 7 (Just n)
presentOnFind rt n [a,b,c,d,e,f,g,h] = h == getName n && assertStateP (seven b c d e f g h findP rt ldN ldN ldN ldN ldN ldN ldN ldN 0) 8 [(sevenK a b c d e f g,n)] && assertStateP (seven b c d e f g h findUnambiguousP rt ldN ldN ldN ldN ldN ldN ldN ldN 0) 8 (Just n)

presentOnFindAll :: Node -> Node -> [String] -> Bool
presentOnFindAll rt n xs@[_]               = for (partials $ tail xs) $ \[]                     -> elem (zeroK,               n) $ snd $ zero                         findP rt l               v      
presentOnFindAll rt n xs@[a,_]             = for (partials $ tail xs) $ \[b']                   -> elem (oneK   a,            n) $ snd $ oneP    b'                   findP rt l l             v     
presentOnFindAll rt n xs@[a,b,_]           = for (partials $ tail xs) $ \[b',c']                -> elem (twoK   a b,          n) $ snd $ twoP    b' c'                findP rt l l l           v   
presentOnFindAll rt n xs@[a,b,c,_]         = for (partials $ tail xs) $ \[b',c',d']             -> elem (threeK a b c,        n) $ snd $ threeP  b' c' d'             findP rt l l l l         v   
presentOnFindAll rt n xs@[a,b,c,d,_]       = for (partials $ tail xs) $ \[b',c',d',e']          -> elem (fourK  a b c d,      n) $ snd $ fourP   b' c' d' e'          findP rt l l l l l       v   
presentOnFindAll rt n xs@[a,b,c,d,e,_]     = for (partials $ tail xs) $ \[b',c',d',e',f']       -> elem (fiveK  a b c d e,    n) $ snd $ fiveP   b' c' d' e' f'       findP rt l l l l l l     v   
presentOnFindAll rt n xs@[a,b,c,d,e,f,_]   = for (partials $ tail xs) $ \[b',c',d',e',f',g']    -> elem (sixK   a b c d e f,  n) $ snd $ sixP    b' c' d' e' f' g'    findP rt l l l l l l l   v   
presentOnFindAll rt n xs@[a,b,c,d,e,f,g,_] = for (partials $ tail xs) $ \[b',c',d',e',f',g',h'] -> elem (sevenK a b c d e f g,n) $ snd $ sevenP  b' c' d' e' f' g' h' findP rt l l l l l l l l v

absentByName :: Node -> String -> [String] -> Bool
absentByName rt s xs = for (addSuffix s $ tail xs) func
 where func []              = assertStateP (zero                findP rt l               v) v [] && assertStateP (zero                findUnambiguousP rt l               v) v Nothing
       func [a]             = assertStateP (one   a             findP rt l l             v) v [] && assertStateP (one   a             findUnambiguousP rt l l             v) v Nothing
       func [a,b]           = assertStateP (two   a b           findP rt l l l           v) v [] && assertStateP (two   a b           findUnambiguousP rt l l l           v) v Nothing
       func [a,b,c]         = assertStateP (three a b c         findP rt l l l l         v) v [] && assertStateP (three a b c         findUnambiguousP rt l l l l         v) v Nothing
       func [a,b,c,d]       = assertStateP (four  a b c d       findP rt l l l l l       v) v [] && assertStateP (four  a b c d       findUnambiguousP rt l l l l l       v) v Nothing
       func [a,b,c,d,e]     = assertStateP (five  a b c d e     findP rt l l l l l l     v) v [] && assertStateP (five  a b c d e     findUnambiguousP rt l l l l l l     v) v Nothing
       func [a,b,c,d,e,f]   = assertStateP (six   a b c d e f   findP rt l l l l l l l   v) v [] && assertStateP (six   a b c d e f   findUnambiguousP rt l l l l l l l   v) v Nothing
       func [a,b,c,d,e,f,g] = assertStateP (seven a b c d e f g findP rt l l l l l l l l v) v [] && assertStateP (seven a b c d e f g findUnambiguousP rt l l l l l l l l v) v Nothing

absentByNoChild :: Node -> [String] -> Bool
absentByNoChild rt xs = for (removeChildren rt $ tail xs) $ \r -> func r $ tail xs
 where func r []              = assertStateP (zero                findP r l               v) v [] && assertStateP (zero                findUnambiguousP r l               v) v Nothing
       func r [a]             = assertStateP (one   a             findP r l l             v) v [] && assertStateP (one   a             findUnambiguousP r l l             v) v Nothing
       func r [a,b]           = assertStateP (two   a b           findP r l l l           v) v [] && assertStateP (two   a b           findUnambiguousP r l l l           v) v Nothing
       func r [a,b,c]         = assertStateP (three a b c         findP r l l l l         v) v [] && assertStateP (three a b c         findUnambiguousP r l l l l         v) v Nothing
       func r [a,b,c,d]       = assertStateP (four  a b c d       findP r l l l l l       v) v [] && assertStateP (four  a b c d       findUnambiguousP r l l l l l       v) v Nothing
       func r [a,b,c,d,e]     = assertStateP (five  a b c d e     findP r l l l l l l     v) v [] && assertStateP (five  a b c d e     findUnambiguousP r l l l l l l     v) v Nothing
       func r [a,b,c,d,e,f]   = assertStateP (six   a b c d e f   findP r l l l l l l l   v) v [] && assertStateP (six   a b c d e f   findUnambiguousP r l l l l l l l   v) v Nothing
       func r [a,b,c,d,e,f,g] = assertStateP (seven a b c d e f g findP r l l l l l l l l v) v [] && assertStateP (seven a b c d e f g findUnambiguousP r l l l l l l l l v) v Nothing

for = flip all

partials []     = [[]]
partials (x:xs) = [ y:ys | y <- [Just x, Nothing], ys <- partials xs ]

addSuffix s ls = filter (/=ls) $ f ls
 where f []     = [[]]
       f (x:xs) = [ y:ys | y <- [x,x++s], ys <- f xs ]

removeChildren _ []    = []
removeChildren n (c:cs) = let (removed,rest) = replaceChild n c cs in  removed:rest

replaceChild parent childName [] = (removeChild parent childName,[])
replaceChild parent childName ns = let Just child = getChild parent childName 
                                       childLess = removeChild parent childName
                                       (newChild,rest) = replaceChild child (head ns) (tail ns) 
                                       newParent = addChild childLess newChild
                                   in  (childLess,newParent:rest)
v = ()

ldN :: k -> Node -> State Int Node
ldN _ x = do
  n <- get
  put (n+1)
  return x
  
type M = State ()
  
l :: k -> Node -> M Node
l _ = return


makeNode :: String -> Int -> Int -> [String] -> Node
makeNode rootName nbChildren maxDepth nodeNames = let (Just result,_) = f maxDepth (rootName:nodeNames) in result
 where f _ []     = (Nothing,[])
       f 0 (x:xs) = (Just $ make x, xs)
       f d (x:xs) = let (result,rest) = g nbChildren xs $ make x
                        g 0  ns m = (m,ns)
                        g nb ns m = let g' = g (nb-1) ns'; (c,ns') = f (d-1) ns in g' $ maybe m (addChild m) c
                    in  (Just result,rest)

makeKeys :: Node -> [(Node,[String])]
makeKeys = map (second reverse) . f []
 where f parentKey n = let key = getName n:parentKey
                           p = (n,key)
                           ps = concatMap (f key) $ getChildren n
                       in  (p:ps)