{-# OPTIONS_GHC -F -pgmF htfpp -fno-warn-incomplete-patterns#-}
module Infrastructure.Finder.FinderPropFindingTest where

import Test.Framework
import Control.Arrow(first,second)
import Data.List (nub)

import Infrastructure.Node
import Domain.Root
import Domain.Project
import Domain.ProjectRepo

import Infrastructure.Finder

{-# ANN module "HLint: ignore Use camelCase" #-}
{-# ANN module "HLint: ignore Reduce duplication" #-}
{-# ANN module "HLint: ignore Evaluate" #-}
{-# ANN module "HLint: ignore Use ++" #-}

prop_findNoHints rootN rN tN cN gN pN trN =
 "" `notElem` [rootN,rN,tN,cN,gN,pN,trN] ==>
  let tr = make trN
      trr = addChild emptyTrainRunRepo tr
      p = setTrainRunRepo (make pN) trr
      g = addChild (make gN) p
      c = addChild (make cN) g
      t = addChild (make tN) c
      r = addChild (make rN) t
      root = addChild (make rootN :: Root) r

      pSubmitR = makeProjectSubmitRepo emptySubmitRepo
      pTrainFileR = makeProjectTrainFileRepo emptyTrainFileRepo
      pTrainRunR = makeProjectTrainRunRepo trr
      prs = [pSubmitR,pTrainFileR,pTrainRunR]
      prs1 = map (\x -> (S (Just pN) Z,x)) prs
      prs2 = map (first (S (Just gN))) prs1
      prs3 = map (first (S (Just cN))) prs2
      prs4 = map (first (S (Just tN))) prs3
      prs5 = map (first (S (Just rN))) prs4
      prs6 = map (first (S (Just rootN))) prs5      
      trrN = getName trr in
      
     [(Z,root)] == find zero root
  && [(Z,r)]    == find zero r
  && [(Z,t)]    == find zero t
  && [(Z,c)]    == find zero c
  && [(Z,g)]    == find zero g
  && [(Z,p)]    == find zero p
  && [(Z,trr)]  == find zero trr
  && [(Z,tr)]   == find zero tr

  && [(one rootN,r)] == find one' root
  && [(one rN,t)]    == find one' r
  && [(one tN,c)]    == find one' t
  && [(one cN,g)]    == find one' c
  && [(one gN,p)]    == find one' g
  && prs1            == find one' p
  && [(one trrN,tr)] == find one' trr

  && [(two rootN rN,t)] == find two' root
  && [(two rN tN,c)]    == find two' r
  && [(two tN cN,g)]    == find two' t
  && [(two cN gN,p)]    == find two' c
  && prs2               == find two' g
  && [(two pN trrN,tr)] == find two' p

  && [(three rootN rN tN,c)] == find three' root
  && [(three rN tN cN,g)]    == find three' r
  && [(three tN cN gN,p)]    == find three' t
  && prs3                    == find three' c
  && [(three gN pN trrN,tr)] == find three' g

  && [(four rootN rN tN cN,g)]  == find four' root
  && [(four rN tN cN gN,p)]     == find four' r
  && prs4                       == find four' t
  && [(four cN gN pN trrN, tr)] == find four' c

  && [(five rootN rN tN cN gN,p)] == find five' root
  && prs5                         == find five' r
  && [(five tN cN gN pN trrN,tr)] == find five' t

  && prs6                        == find six' root
  && [(six rN tN cN gN pN trrN,tr)] == find six' r

  && [(seven rootN rN tN cN gN pN trrN,tr)] == find seven' root

prop_findAllHints rootN rN tN cN gN pN trN =
 "" `notElem` [rootN,rN,tN,cN,gN,pN,trN] ==>
  let tr = make trN
      trr = addChild emptyTrainRunRepo tr
      pSubmitR = makeProjectSubmitRepo emptySubmitRepo
      pTrainFileR = makeProjectTrainFileRepo emptyTrainFileRepo
      pTrainRunR = makeProjectTrainRunRepo trr
      p = setTrainRunRepo (make pN) trr
      g = addChild (make gN) p
      c = addChild (make cN) g
      t = addChild (make tN) c
      r = addChild (make rN) t
      root = addChild (make rootN :: Root) r

      sRN = getName emptySubmitRepo
      tfRN = getName emptyTrainFileRepo
      trRN = getName emptyTrainRunRepo in
     [(one rootN,r)]        == find (one rN) root
  && [(one rN,t)]           == find (one tN) r
  && [(one tN,c)]           == find (one cN) t
  && [(one cN,g)]           == find (one gN) c
  && [(one gN,p)]           == find (one pN) g
  && [(one pN,pSubmitR)]    == find (one sRN) p
  && [(one pN,pTrainFileR)] == find (one tfRN) p
  && [(one pN,pTrainRunR)]  == find (one trRN) p
  && [(one trRN,tr)]        == find (one trN) trr

  && [(two rootN rN,t)]        == find (two rN tN) root
  && [(two rN tN,c)]           == find (two tN cN) r
  && [(two tN cN,g)]           == find (two cN gN) t
  && [(two cN gN,p)]           == find (two gN pN) c
  && [(two gN pN,pSubmitR)]    == find (two pN sRN) g
  && [(two gN pN,pTrainFileR)] == find (two pN tfRN) g
  && [(two gN pN,pTrainRunR)]  == find (two pN trRN) g
  && [(two pN trRN,tr)]        == find (two trRN trN) p
  
  && [(three rootN rN tN,c)]        == find (three rN tN cN) root
  && [(three rN tN cN,g)]           == find (three tN cN gN) r
  && [(three tN cN gN,p)]           == find (three cN gN pN) t
  && [(three cN gN pN,pSubmitR)]    == find (three gN pN sRN) c
  && [(three cN gN pN,pTrainFileR)] == find (three gN pN tfRN) c
  && [(three cN gN pN,pTrainRunR)]  == find (three gN pN trRN) c
  && [(three gN pN trRN,tr)]        == find (three pN trRN trN) g
  
  && [(four rootN rN tN cN,g)]        == find (four rN tN cN gN) root
  && [(four rN tN cN gN,p)]           == find (four tN cN gN pN) r
  && [(four tN cN gN pN,pSubmitR)]    == find (four cN gN pN sRN) t
  && [(four tN cN gN pN,pTrainFileR)] == find (four cN gN pN tfRN) t
  && [(four tN cN gN pN,pTrainRunR)]  == find (four cN gN pN trRN) t
  && [(four cN gN pN trRN,tr)]        == find (four gN pN trRN trN) c
  
  && [(five rootN rN tN cN gN,p)]        == find (five rN tN cN gN pN) root
  && [(five rN tN cN gN pN,pSubmitR)]    == find (five tN cN gN pN sRN) r
  && [(five rN tN cN gN pN,pTrainFileR)] == find (five tN cN gN pN tfRN) r
  && [(five rN tN cN gN pN,pTrainRunR)]  == find (five tN cN gN pN trRN) r
  && [(five tN cN gN pN trRN, tr)]             == find (five cN gN pN trRN trN) t
  
  && [(six rootN rN tN cN gN pN,pSubmitR)]    == find (six rN tN cN gN pN sRN) root
  && [(six rootN rN tN cN gN pN,pTrainFileR)] == find (six rN tN cN gN pN tfRN) root
  && [(six rootN rN tN cN gN pN,pTrainRunR)]  == find (six rN tN cN gN pN trRN) root
  && [(six rN tN cN gN pN trRN,tr)]          == find (six tN cN gN pN trRN trN) r
  
  && [(seven rootN rN tN cN gN pN trRN,tr)]          == find (seven rN tN cN gN pN trRN trN) root

prop_findGeneral rootName ns = let names = filter (not.null) $ nub ns in names /= [] ==>
  let root = makeNode rootName 4 7 names
  in  all (uncurry (presentOnFind root)) $ makeKeys root
  
presentOnFind :: Node -> Node -> [String] -> Bool                                                                                                                     
presentOnFind rt n [roo]                 = let z = (zero,n)                   in getName n == roo && [z] == find zero rt                                                                                   
presentOnFind rt n [roo,r]               = let z = (one roo,n)                in getName n == r   && [z] == find (one r) rt                 && all (z `elem`) (map (`find`rt) $ partialOne r)
presentOnFind rt n [roo,r,t]             = let z = (two roo r,n)              in getName n == t   && [z] == find (two r t) rt               && all (z `elem`) (map (`find`rt) $ partialTwo r t)
presentOnFind rt n [roo,r,t,c]           = let z = (three roo r t,n)          in getName n == c   && [z] == find (three r t c) rt           && all (z `elem`) (map (`find`rt) $ partialThree r t c)
presentOnFind rt n [roo,r,t,c,g]         = let z = (four roo r t c,n)         in getName n == g   && [z] == find (four r t c g) rt          && all (z `elem`) (map (`find`rt) $ partialFour r t c g)
presentOnFind rt n [roo,r,t,c,g,p]       = let z = (five roo r t c g,n)       in getName n == p   && [z] == find (five r t c g p) rt        && all (z `elem`) (map (`find`rt) $ partialFive r t c g p)
presentOnFind rt n [roo,r,t,c,g,p,pr]    = let z = (six roo r t c g p,n)      in getName n == pr  && [z] == find (six r t c g p pr) rt      && all (z `elem`) (map (`find`rt) $ partialSix r t c g p pr)
presentOnFind rt n [roo,r,t,c,g,p,pr,tr] = let z = (seven roo r t c g p pr,n) in getName n == tr  && [z] == find (seven r t c g p pr tr) rt && all (z `elem`) (map (`find`rt) $ partialSeven r t c g p pr tr)

partialOne a               = [S x Z | x <- [Just a, Nothing]]
partialTwo a b             = [S x s | x <- [Just a, Nothing], s <- partialOne b]
partialThree a b c         = [S x s | x <- [Just a, Nothing], s <- partialTwo b c]
partialFour a b c d        = [S x s | x <- [Just a, Nothing], s <- partialThree b c d]
partialFive a b c d e      = [S x s | x <- [Just a, Nothing], s <- partialFour b c d e]
partialSix a b c d e f     = [S x s | x <- [Just a, Nothing], s <- partialFive b c d e f]
partialSeven a b c d e f g = [S x s | x <- [Just a, Nothing], s <- partialSix b c d e f g]

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

one' = S Nothing zero
two' = S Nothing one'
three' = S Nothing two'
four' = S Nothing three'
five' = S Nothing four'
six' = S Nothing five'
seven' = S Nothing six'

zero = Z
one a = S (Just a) zero
two a b = S (Just a) $ one b
three a b c = S (Just a) $ two b c
four a b c d = S (Just a) $ three b c d
five a b c d e = S (Just a) $ four b c d e
six a b c d e f = S (Just a) $ five b c d e f
seven a b c d e f g = S (Just a) $ six b c d e f g


