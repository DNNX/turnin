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
      prs1 = map (\x -> (K pN Z,x)) prs
      prs2 = map (\x -> (K pN $ K gN Z,x)) prs
      prs3 = map (\x -> (K pN $ K gN $ K cN Z,x)) prs
      prs4 = map (\x -> (K pN $ K gN $ K cN $ K tN Z,x)) prs
      prs5 = map (\x -> (K pN $ K gN $ K cN $ K tN $ K rN Z,x)) prs
      prs6 = map (\x -> (K pN $ K gN $ K cN $ K tN $ K rN $ K rootN Z,x)) prs
      trrN = getName trr in

     [(Z,root)] == find zero root                                 && Just root == findUnambiguous zero root
  && [(Z,r)]    == find zero r                                    && Just r    == findUnambiguous zero r
  && [(Z,t)]    == find zero t                                    && Just t    == findUnambiguous zero t
  && [(Z,c)]    == find zero c                                    && Just c    == findUnambiguous zero c
  && [(Z,g)]    == find zero g                                    && Just g    == findUnambiguous zero g
  && [(Z,p)]    == find zero p                                    && Just p    == findUnambiguous zero p
  && [(Z,trr)]  == find zero trr                                  && Just trr  == findUnambiguous zero trr
  && [(Z,tr)]   == find zero tr                                   && Just tr   == findUnambiguous zero tr

  && [(one rootN,r)] == find one' root                            && Just r == findUnambiguous one' root
  && [(one rN,t)]    == find one' r                               && Just t == findUnambiguous one' r
  && [(one tN,c)]    == find one' t                               && Just c == findUnambiguous one' t
  && [(one cN,g)]    == find one' c                               && Just g == findUnambiguous one' c
  && [(one gN,p)]    == find one' g                               && Just p == findUnambiguous one' g
  && prs1            == find one' p
  && [(one trrN,tr)] == find one' trr                             && Just tr == findUnambiguous one' trr

  && [(two rootN rN,t)] == find two' root                         && Just t == findUnambiguous two' root
  && [(two rN tN,c)]    == find two' r                            && Just c == findUnambiguous two' r
  && [(two tN cN,g)]    == find two' t                            && Just g == findUnambiguous two' t
  && [(two cN gN,p)]    == find two' c                            && Just p == findUnambiguous two' c
  && prs2               == find two' g
  && [(two pN trrN,tr)] == find two' p                            && Just tr == findUnambiguous two' p

  && [(three rootN rN tN,c)] == find three' root                  && Just c == findUnambiguous three' root
  && [(three rN tN cN,g)]    == find three' r                     && Just g == findUnambiguous three' r
  && [(three tN cN gN,p)]    == find three' t                     && Just p == findUnambiguous three' t
  && prs3                    == find three' c
  && [(three gN pN trrN,tr)] == find three' g                     && Just tr == findUnambiguous three' g

  && [(four rootN rN tN cN,g)]  == find four' root                && Just g == findUnambiguous four' root
  && [(four rN tN cN gN,p)]     == find four' r                   && Just p == findUnambiguous four' r
  && prs4                       == find four' t 
  && [(four cN gN pN trrN, tr)] == find four' c                   && Just tr == findUnambiguous four' c

  && [(five rootN rN tN cN gN,p)] == find five' root              && Just p == findUnambiguous five' root
  && prs5                         == find five' r  
  && [(five tN cN gN pN trrN,tr)] == find five' t                 && Just tr == findUnambiguous five' t

  && prs6                        == find six' root
  && [(six rN tN cN gN pN trrN,tr)] == find six' r                && Just tr == findUnambiguous six' r

  && [(seven rootN rN tN cN gN pN trrN,tr)] == find seven' root   && Just tr == findUnambiguous seven' root

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
     [(one rootN,r)]        == find (oneM rN) root                                           && Just r           == findUnambiguous (oneM rN) root                                         
  && [(one rN,t)]           == find (oneM tN) r                                              && Just t           == findUnambiguous (oneM tN) r                                            
  && [(one tN,c)]           == find (oneM cN) t                                              && Just c           == findUnambiguous (oneM cN) t                                            
  && [(one cN,g)]           == find (oneM gN) c                                              && Just g           == findUnambiguous (oneM gN) c                                            
  && [(one gN,p)]           == find (oneM pN) g                                              && Just p           == findUnambiguous (oneM pN) g                                            
  && [(one pN,pSubmitR)]    == find (oneM sRN) p                                             && Just pSubmitR    == findUnambiguous (oneM sRN) p                                           
  && [(one pN,pTrainFileR)] == find (oneM tfRN) p                                            && Just pTrainFileR == findUnambiguous (oneM tfRN) p                                          
  && [(one pN,pTrainRunR)]  == find (oneM trRN) p                                            && Just pTrainRunR  == findUnambiguous (oneM trRN) p                                          
  && [(one trRN,tr)]        == find (oneM trN) trr                                           && Just tr          == findUnambiguous (oneM trN) trr                                         
                                                                                                                                                                               
  && [(two rootN rN,t)]        == find (twoM rN tN) root                                     && Just t           == findUnambiguous (twoM rN tN) root                                   
  && [(two rN tN,c)]           == find (twoM tN cN) r                                        && Just c           == findUnambiguous (twoM tN cN) r                                      
  && [(two tN cN,g)]           == find (twoM cN gN) t                                        && Just g           == findUnambiguous (twoM cN gN) t                                      
  && [(two cN gN,p)]           == find (twoM gN pN) c                                        && Just p           == findUnambiguous (twoM gN pN) c                                      
  && [(two gN pN,pSubmitR)]    == find (twoM pN sRN) g                                       && Just pSubmitR    == findUnambiguous (twoM pN sRN) g                                     
  && [(two gN pN,pTrainFileR)] == find (twoM pN tfRN) g                                      && Just pTrainFileR == findUnambiguous (twoM pN tfRN) g                                    
  && [(two gN pN,pTrainRunR)]  == find (twoM pN trRN) g                                      && Just pTrainRunR  == findUnambiguous (twoM pN trRN) g                                    
  && [(two pN trRN,tr)]        == find (twoM trRN trN) p                                     && Just tr          == findUnambiguous (twoM trRN trN) p                                   
                                                                                                                                                                          
  && [(three rootN rN tN,c)]        == find (threeM rN tN cN) root                           && Just  c           == findUnambiguous (threeM rN tN cN) root                         
  && [(three rN tN cN,g)]           == find (threeM tN cN gN) r                              && Just  g           == findUnambiguous (threeM tN cN gN) r                            
  && [(three tN cN gN,p)]           == find (threeM cN gN pN) t                              && Just  p           == findUnambiguous (threeM cN gN pN) t                            
  && [(three cN gN pN,pSubmitR)]    == find (threeM gN pN sRN) c                             && Just  pSubmitR    == findUnambiguous (threeM gN pN sRN) c                           
  && [(three cN gN pN,pTrainFileR)] == find (threeM gN pN tfRN) c                            && Just  pTrainFileR == findUnambiguous (threeM gN pN tfRN) c                          
  && [(three cN gN pN,pTrainRunR)]  == find (threeM gN pN trRN) c                            && Just  pTrainRunR  == findUnambiguous (threeM gN pN trRN) c                          
  && [(three gN pN trRN,tr)]        == find (threeM pN trRN trN) g                           && Just  tr          == findUnambiguous (threeM pN trRN trN) g                         
                                                                                                                                                                              
  && [(four rootN rN tN cN,g)]        == find (fourM rN tN cN gN) root                       && Just g           == findUnambiguous (fourM rN tN cN gN) root                     
  && [(four rN tN cN gN,p)]           == find (fourM tN cN gN pN) r                          && Just p           == findUnambiguous (fourM tN cN gN pN) r                        
  && [(four tN cN gN pN,pSubmitR)]    == find (fourM cN gN pN sRN) t                         && Just pSubmitR    == findUnambiguous (fourM cN gN pN sRN) t                       
  && [(four tN cN gN pN,pTrainFileR)] == find (fourM cN gN pN tfRN) t                        && Just pTrainFileR == findUnambiguous (fourM cN gN pN tfRN) t                      
  && [(four tN cN gN pN,pTrainRunR)]  == find (fourM cN gN pN trRN) t                        && Just pTrainRunR  == findUnambiguous (fourM cN gN pN trRN) t                      
  && [(four cN gN pN trRN,tr)]        == find (fourM gN pN trRN trN) c                       && Just tr          == findUnambiguous (fourM gN pN trRN trN) c                     
                                                                                                                                                                           
  && [(five rootN rN tN cN gN,p)]        == find (fiveM rN tN cN gN pN) root                 && Just p           == findUnambiguous (fiveM rN tN cN gN pN) root               
  && [(five rN tN cN gN pN,pSubmitR)]    == find (fiveM tN cN gN pN sRN) r                   && Just pSubmitR    == findUnambiguous (fiveM tN cN gN pN sRN) r                 
  && [(five rN tN cN gN pN,pTrainFileR)] == find (fiveM tN cN gN pN tfRN) r                  && Just pTrainFileR == findUnambiguous (fiveM tN cN gN pN tfRN) r                
  && [(five rN tN cN gN pN,pTrainRunR)]  == find (fiveM tN cN gN pN trRN) r                  && Just pTrainRunR  == findUnambiguous (fiveM tN cN gN pN trRN) r                
  && [(five tN cN gN pN trRN, tr)]       == find (fiveM cN gN pN trRN trN) t                 && Just tr          == findUnambiguous (fiveM cN gN pN trRN trN) t               
                                                                                                                                                                          
  && [(six rootN rN tN cN gN pN,pSubmitR)]    == find (sixM rN tN cN gN pN sRN) root         && Just pSubmitR    == findUnambiguous (sixM rN tN cN gN pN sRN) root       
  && [(six rootN rN tN cN gN pN,pTrainFileR)] == find (sixM rN tN cN gN pN tfRN) root        && Just pTrainFileR == findUnambiguous (sixM rN tN cN gN pN tfRN) root      
  && [(six rootN rN tN cN gN pN,pTrainRunR)]  == find (sixM rN tN cN gN pN trRN) root        && Just pTrainRunR  == findUnambiguous (sixM rN tN cN gN pN trRN) root      
  && [(six rN tN cN gN pN trRN,tr)]           == find (sixM tN cN gN pN trRN trN) r          && Just tr          == findUnambiguous (sixM tN cN gN pN trRN trN) r        
                                                                                                                                                                           
  && [(seven rootN rN tN cN gN pN trRN,tr)]   == find (sevenM rN tN cN gN pN trRN trN) root  && Just tr          == findUnambiguous (sevenM rN tN cN gN pN trRN trN) root

prop_findGeneral rootName ns = let names = filter (not.null) $ nub ns in names /= [] ==>
  let root = makeNode rootName 4 7 names
  in  all (uncurry (presentOnFind root)) $ makeKeys root

presentOnFind :: Node -> Node -> [String] -> Bool                                                                                                      
presentOnFind rt n [roo]                 = let z = (zero,n)                   in getName n == roo && [z] == find zero rt                                                                                       && Just n == findUnambiguous zero rt                    
presentOnFind rt n [roo,r]               = let z = (one roo,n)                in getName n == r   && [z] == find (oneM r) rt                 && all (z `elem`) (map (`find`rt) $ partialOne r)                 && Just n == findUnambiguous (oneM r) rt                
presentOnFind rt n [roo,r,t]             = let z = (two roo r,n)              in getName n == t   && [z] == find (twoM r t) rt               && all (z `elem`) (map (`find`rt) $ partialTwo r t)               && Just n == findUnambiguous (twoM r t) rt              
presentOnFind rt n [roo,r,t,c]           = let z = (three roo r t,n)          in getName n == c   && [z] == find (threeM r t c) rt           && all (z `elem`) (map (`find`rt) $ partialThree r t c)           && Just n == findUnambiguous (threeM r t c) rt          
presentOnFind rt n [roo,r,t,c,g]         = let z = (four roo r t c,n)         in getName n == g   && [z] == find (fourM r t c g) rt          && all (z `elem`) (map (`find`rt) $ partialFour r t c g)          && Just n == findUnambiguous (fourM r t c g) rt         
presentOnFind rt n [roo,r,t,c,g,p]       = let z = (five roo r t c g,n)       in getName n == p   && [z] == find (fiveM r t c g p) rt        && all (z `elem`) (map (`find`rt) $ partialFive r t c g p)        && Just n == findUnambiguous (fiveM r t c g p) rt       
presentOnFind rt n [roo,r,t,c,g,p,pr]    = let z = (six roo r t c g p,n)      in getName n == pr  && [z] == find (sixM r t c g p pr) rt      && all (z `elem`) (map (`find`rt) $ partialSix r t c g p pr)      && Just n == findUnambiguous (sixM r t c g p pr) rt     
presentOnFind rt n [roo,r,t,c,g,p,pr,tr] = let z = (seven roo r t c g p pr,n) in getName n == tr  && [z] == find (sevenM r t c g p pr tr) rt && all (z `elem`) (map (`find`rt) $ partialSeven r t c g p pr tr) && Just n == findUnambiguous (sevenM r t c g p pr tr) rt

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
one a               = K a zero
two a b             = K b $ one a
three a b c         = K c $ two a b
four a b c d        = K d $ three a b c
five a b c d e      = K e $ four a b c d
six a b c d e f     = K f $ five a b c d e
seven a b c d e f g = K g $ six a b c d e f

oneM a               = S (Just a) zero
twoM a b             = S (Just a) $ oneM b
threeM a b c         = S (Just a) $ twoM b c
fourM a b c d        = S (Just a) $ threeM b c d
fiveM a b c d e      = S (Just a) $ fourM b c d e
sixM a b c d e f     = S (Just a) $ fiveM b c d e f
sevenM a b c d e f g = S (Just a) $ sixM b c d e f g


