{-# OPTIONS_GHC -F -pgmF htfpp -fno-warn-incomplete-patterns#-}
module Infrastructure.Finder.FinderUnitNoFindingTest where

import Test.Framework
import Data.List hiding (find)
import Control.Arrow (first)

import Infrastructure.Node
import Domain.Root
import Domain.Project
import Domain.ProjectRepo

import Infrastructure.Finder

{-# ANN module "HLint: ignore Use camelCase" #-}
{-# ANN module "HLint: ignore Reduce duplication" #-}

test_noFindNoHints = do
  let rootN = "root"
      rN = "repo"
      tN = "term"
      cN = "course"
      gN = "group"
      pN = "project"
      trN = "trainRun"

      rt0 = make rootN :: Root
      [rt1,rt2,rt3,rt4,rt5,rt6,_] = map (addChild rt0) [r0,r1,r2,r3,r4,r5,r6]
      r0 = make rN
      [r1,r2,r3,r4,r5,r6] = map (addChild r0) [t0,t1,t2,t3,t4,t5]
      t0 = make tN
      [t1,t2,t3,t4,t5] = map (addChild t0) [c0,c1,c2,c3,c4]
      c0 = make cN
      [c1,c2,c3,c4] = map (addChild c0) [g0,g1,g2,g3]
      g0 = make gN
      [g1,g2,g3] = map (addChild g0) [p0,p1,p2]
      p0 = make pN
      [p1,p2] = map (setTrainRunRepo p0) [trr0,trr1]
      trr0 = emptyTrainRunRepo
      [trr1] = map (addChild trr0) [tr0]
      tr0 = make trN

      prs1 = map (\x -> (K pN Z,x)) [pSubmitR,pTrainFileR,pTrainRunR]
      prs2 = map (first (K gN)) prs1
      prs3 = map (first (K cN)) prs2
      prs4 = map (first (K tN)) prs3
      prs5 = map (first (K rN)) prs4
      prs6 = map (first (K rootN)) prs5
      pSubmitR = makeProjectSubmitRepo emptySubmitRepo
      pTrainFileR = makeProjectTrainFileRepo emptyTrainFileRepo
      pTrainRunR = makeProjectTrainRunRepo trr0

  assertEqual [[]] $ nub $ map (find one') [rt0]
  assertEqual [[]] $ nub $ map (find one') [r0]
  assertEqual [[]] $ nub $ map (find one') [t0]
  assertEqual [[]] $ nub $ map (find one') [c0]
  assertEqual [[]] $ nub $ map (find one') [g0]
--  assertEqual [[]] $ nub $ map (find one') []
  assertEqual [[]] $ nub $ map (find one') [trr0]
  assertEqual prs1 $ find one' p0
  
  assertEqual [[]] $ nub $ map (find two') [rt0,rt1]
  assertEqual [[]] $ nub $ map (find two') [r0,r1]
  assertEqual [[]] $ nub $ map (find two') [t0,t1]
  assertEqual [[]] $ nub $ map (find two') [c0,c1]
  assertEqual [[]] $ nub $ map (find two') [g0]
  assertEqual [[]] $ nub $ map (find two') [p0,p1]
  assertEqual prs2 $ find two' g1
  
  assertEqual [[]] $ nub $ map (find three') [rt0,rt1,rt2]
  assertEqual [[]] $ nub $ map (find three') [r0,r1,r2]
  assertEqual [[]] $ nub $ map (find three') [t0,t1,t2]
  assertEqual [[]] $ nub $ map (find three') [c0,c1]
  assertEqual [[]] $ nub $ map (find three') [g0,g1,g2]
  assertEqual prs3 $ find three' c2
  
  assertEqual [[]] $ nub $ map (find four') [rt0,rt1,rt2,rt3]
  assertEqual [[]] $ nub $ map (find four') [r0,r1,r2,r3]
  assertEqual [[]] $ nub $ map (find four') [t0,t1,t2]
  assertEqual [[]] $ nub $ map (find four') [c0,c1,c2,c3]
  assertEqual prs4 $ find four' t3
  
  assertEqual [[]] $ nub $ map (find five') [rt0,rt1,rt2,rt3,rt4]
  assertEqual [[]] $ nub $ map (find five') [r0,r1,r2,r3]
  assertEqual [[]] $ nub $ map (find five') [t0,t1,t2,t3,t4]
  assertEqual prs5 $ find five' r4
  
  assertEqual [[]] $ nub $ map (find six') [rt0,rt1,rt2,rt3,rt4]
  assertEqual [[]] $ nub $ map (find six') [r0,r1,r2,r3,r4,r5]
  assertEqual prs6 $ find six' rt5
  
  assertEqual [[]] $ nub $ map (find seven') [rt0,rt1,rt2,rt3,rt4,rt5,rt6]

test_noFindAllGoodHints = do
  let rootN = "root"
      rN = "repo"
      tN = "term"
      cN = "course"
      gN = "group"
      pN = "project"
      trN = "trainRun"
      trRN = getName emptyTrainRunRepo

      rt0 = make rootN :: Root
      [rt1,rt2,rt3,rt4,rt5,rt6,_] = map (addChild rt0) [r0,r1,r2,r3,r4,r5,r6]
      r0 = make rN
      [r1,r2,r3,r4,r5,r6] = map (addChild r0) [t0,t1,t2,t3,t4,t5]
      t0 = make tN
      [t1,t2,t3,t4,t5] = map (addChild t0) [c0,c1,c2,c3,c4]
      c0 = make cN
      [c1,c2,c3,c4] = map (addChild c0) [g0,g1,g2,g3]
      g0 = make gN
      [g1,g2,g3] = map (addChild g0) [p0,p1,p2]
      p0 = make pN
      [p1,p2] = map (setTrainRunRepo p0) [trr0,trr1]
      trr0 = emptyTrainRunRepo
      [trr1] = map (addChild trr0) [tr0]
      tr0 = make trN

  assertEqual [[]] $ nub $ map (find (one rN))  [rt0] 
  assertEqual [[]] $ nub $ map (find (one tN))  [r0]
  assertEqual [[]] $ nub $ map (find (one cN))  [t0]
  assertEqual [[]] $ nub $ map (find (one gN))  [c0]
  assertEqual [[]] $ nub $ map (find (one pN))  [g0]
  assertEqual [[]] $ nub $ map (find (one trN)) [trr0]
  
  assertEqual [[]] $ nub $ map (find (two rN tN))    [rt0,rt1] 
  assertEqual [[]] $ nub $ map (find (two tN cN))    [r0,r1]
  assertEqual [[]] $ nub $ map (find (two cN gN))    [t0,t1]
  assertEqual [[]] $ nub $ map (find (two gN pN))    [c0,c1]
  assertEqual [[]] $ nub $ map (find (two pN trRN))  [p0,p1]
  
  assertEqual [[]] $ nub $ map (find (three rN tN cN))    [rt0,rt1,rt2]
  assertEqual [[]] $ nub $ map (find (three tN cN gN))    [r0,r1,r2]
  assertEqual [[]] $ nub $ map (find (three cN gN pN))    [t0,t1,t2]
  assertEqual [[]] $ nub $ map (find (three pN trRN trN)) [g0,g1,g2]
  
  assertEqual [[]] $ nub $ map (find (four rN tN cN gN))    [rt0,rt1,rt2,rt3]
  assertEqual [[]] $ nub $ map (find (four tN cN gN pN))    [r0,r1,r2,r3]
  assertEqual [[]] $ nub $ map (find (four cN gN pN trN))   [t0,t1,t2,t3]
  assertEqual [[]] $ nub $ map (find (four gN pN trRN trN)) [c0,c1,c2,c3]
  
  assertEqual [[]] $ nub $ map (find (five rN tN cN gN pN))    [rt0,rt1,rt2,rt3,rt4]
  assertEqual [[]] $ nub $ map (find (five tN cN gN pN trN))   [r0,r1,r2,r3,r4]
  assertEqual [[]] $ nub $ map (find (five cN gN pN trRN trN)) [t0,t1,t2,t3,t4]
  
  assertEqual [[]] $ nub $ map (find (six rN cN gN pN trRN trN))  [rt0,rt1,rt2,rt3,rt4,rt5]
  assertEqual [[]] $ nub $ map (find (six cN gN pN trRN trN trN)) [r0,r1,r2,r3,r4,r5]
  
  assertEqual [[]] $ nub $ map (find (seven rN tN cN gN pN trRN trN)) [rt0,rt1,rt2,rt3,rt4,rt5,rt6]

test_noFindAllBadHints = do
  let tr = make $ trN ++ "0"
      trr = addChild emptyTrainRunRepo tr
      p = setTrainRunRepo (make $ pN ++ "0") trr
      g = addChild (make $ gN ++ "0") p
      c = addChild (make $ cN ++ "0") g
      t = addChild (make $ tN ++ "0") c
      r = addChild (make $ rN ++ "0") t
      rt = addChild (make (rtN ++ "0") :: Root) r

      rtN = "root"
      rN = "repo"
      tN = "term"
      cN = "course"
      gN = "group"
      pN = "project"
      trN = "trainRun"
      sRN = getName emptySubmitRepo ++ "0"
      tfRN = getName emptyTrainFileRepo ++ "0"
      trRN = getName emptyTrainRunRepo ++ "0"

  assertEqual [] $ find (one rN) rt
  assertEqual [] $ find (one tN) r    
  assertEqual [] $ find (one cN) t  
  assertEqual [] $ find (one gN) c    
  assertEqual [] $ find (one pN) g    
  assertEqual [] $ find (one sRN) p   
  assertEqual [] $ find (one tfRN) p  
  assertEqual [] $ find (one trRN) p  
  assertEqual [] $ find (one trN) trr 
  
  assertEqual [] $ find (two rN tN) rt
  assertEqual [] $ find (two tN cN) r   
  assertEqual [] $ find (two cN gN) t   
  assertEqual [] $ find (two gN pN) c  
  assertEqual [] $ find (two pN sRN) g 
  assertEqual [] $ find (two pN tfRN) g
  assertEqual [] $ find (two pN trRN) g
  assertEqual [] $ find (two pN trRN) p 
  
  assertEqual [] $ find (three rN tN cN) rt
  assertEqual [] $ find (three tN cN gN) r    
  assertEqual [] $ find (three cN gN pN) t    
  assertEqual [] $ find (three gN pN sRN) c   
  assertEqual [] $ find (three gN pN tfRN) c
  assertEqual [] $ find (three gN pN trRN) c  
  assertEqual [] $ find (three pN trRN trN) g
  
  assertEqual [] $ find (four rN tN cN gN) rt
  assertEqual [] $ find (four tN cN gN pN) r    
  assertEqual [] $ find (four cN gN pN sRN) t   
  assertEqual [] $ find (four cN gN pN tfRN) t  
  assertEqual [] $ find (four cN gN pN trRN) t  
  assertEqual [] $ find (four gN pN trRN trN) c 
  
  assertEqual [] $ find (five rN tN cN gN pN) rt
  assertEqual [] $ find (five tN cN gN pN sRN) r
  assertEqual [] $ find (five tN cN gN pN tfRN) r   
  assertEqual [] $ find (five tN cN gN pN trRN) r   
  assertEqual [] $ find (five cN gN pN trRN trN) t  
  
  assertEqual [] $ find (six rN tN cN gN pN trRN) rt
  assertEqual [] $ find (six rN tN cN gN pN tfRN) rt
  assertEqual [] $ find (six rN tN cN gN pN sRN) rt
  assertEqual [] $ find (six rN cN gN pN trRN trN) r 
  
  assertEqual [] $ find (seven rN tN cN gN pN trRN trN) rt

one'   = S Nothing zero
two'   = S Nothing one'
three' = S Nothing two'
four'  = S Nothing three'
five'  = S Nothing four'
six'   = S Nothing five'
seven' = S Nothing six'

zero = Z
one a               = S (Just a) zero
two a b             = S (Just a) $ one b
three a b c         = S (Just a) $ two b c
four a b c d        = S (Just a) $ three b c d
five a b c d e      = S (Just a) $ four b c d e
six a b c d e f     = S (Just a) $ five b c d e f
seven a b c d e f g = S (Just a) $ six b c d e f g

