{-# OPTIONS_GHC -F -pgmF htfpp -fno-warn-incomplete-patterns#-}
module Infrastructure.FinderPropNoFindingTest where

import Test.Framework
import Data.List hiding (find)

import Infrastructure.Node
import Domain.Root
import Domain.Project
import Domain.ProjectRepo

import Infrastructure.Finder

{-# ANN module "HLint: ignore Use camelCase" #-}
{-# ANN module "HLint: ignore Reduce duplication" #-}

prop_noFindNoHints rtN rN tN cN gN pN trN =
 "" `notElem` [rtN,rN,tN,cN,gN,pN,trN] ==>
  let rt0 = make rtN :: Root
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

      prs = [pSubmitR,pTrainFileR,pTrainRunR]
      pSubmitR = makeProjectSubmitRepo emptySubmitRepo
      pTrainFileR = makeProjectTrainFileRepo emptyTrainFileRepo
      pTrainRunR = makeProjectTrainRunRepo trr0 in
  [[]]  == (nub $ map (find one') [rt0]) &&
  [[]]  == (nub $ map (find one') [r0] ++ map (find two') [rt0,rt1]) &&
  [[]]  == (nub $ map (find one') [t0] ++ map (find two') [r0,r1] ++ map (find three') [rt0,rt1,rt2]) &&
  [[]]  == (nub $ map (find one') [c0] ++ map (find two') [t0,t1] ++ map (find three') [r0,r1,r2] ++ map (find four') [rt0,rt1,rt2,rt3]) &&
  [[]]  == (nub $ map (find one') [g0] ++ map (find two') [c0,c1] ++ map (find three') [t0,t1,t2] ++ map (find four') [r0,r1,r2,r3] ++ map (find five') [rt0,rt1,rt2,rt3,rt4]) &&
       
  [prs] == (nub $ map (find one') [p0] ++ map (find two') [g1]    ++ map (find three') [c2]       ++ map (find four') [t3]          ++ map (find five') [r4]          ++ map (find six') [rt5]) &&
  [[]]  == (nub $                         map (find two') [g0]    ++ map (find three') [c0,c1]    ++ map (find four') [t0,t1,t2]    ++ map (find five') [r0,r1,r2,r3] ++ map (find six') [rt0,rt1,rt2,rt3,rt4]) &&
        
  [[]]  == (nub $ map (find one') [trr0] ++ map (find two') [p0,p1] ++ map (find three') [g0,g1,g2] ++ map (find four') [c0,c1,c2,c3] ++ map (find five') [t0,t1,t2,t3,t4] ++ map (find six') [r0,r1,r2,r3,r4,r5] ++ map (find seven') [rt0,rt1,rt2,rt3,rt4,rt5,rt6])

prop_noFindAllGoodHints rtN rN tN cN gN pN trN =
 "" `notElem` [rtN,rN,tN,cN,gN,pN,trN] ==>
  let trRN = getName emptyTrainRunRepo

      rt0 = make rtN :: Root
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
      tr0 = make trN in

  [[]]  == (nub $ map (find (one rN))  [rt0]) &&
  [[]]  == (nub $ map (find (one tN))  [r0]   ++ map (find (two rN tN))   [rt0,rt1]) &&
  [[]]  == (nub $ map (find (one cN))  [t0]   ++ map (find (two tN cN))   [r0,r1] ++ map (find (three rN tN cN))    [rt0,rt1,rt2]) &&
  [[]]  == (nub $ map (find (one gN))  [c0]   ++ map (find (two cN gN))   [t0,t1] ++ map (find (three tN cN gN))    [r0,r1,r2] ++ map (find (four rN tN cN gN))    [rt0,rt1,rt2,rt3]) &&
  [[]]  == (nub $ map (find (one pN))  [g0]   ++ map (find (two gN pN))   [c0,c1] ++ map (find (three cN gN pN))    [t0,t1,t2] ++ map (find (four tN cN gN pN))    [r0,r1,r2,r3] ++ map (find (five rN tN cN gN pN))    [rt0,rt1,rt2,rt3,rt4]) &&
  [[]]  == (nub $ map (find (one trN)) [trr0] ++ map (find (two pN trRN)) [p0,p1] ++ map (find (three pN trRN trN)) [g0,g1,g2] ++ map (find (four gN pN trRN trN)) [c0,c1,c2,c3] ++ map (find (five cN gN pN trRN trN)) [t0,t1,t2,t3,t4]     ++ map (find (six rN cN gN pN trRN trN)) [r0,r1,r2,r3,r4,r5] ++ map (find (seven rN tN cN gN pN trRN trN)) [rt0,rt1,rt2,rt3,rt4,rt5,rt6])

prop_noFindAllBadHints rtN rN tN cN gN pN trN suffix =
 "" `notElem` [rtN,rN,tN,cN,gN,pN,trN,suffix] ==>
  let tr = make $ trN ++ suffix
      trr = addChild emptyTrainRunRepo tr
      p = setTrainRunRepo (make $ pN ++ suffix) trr
      g = addChild (make $ gN ++ suffix) p
      c = addChild (make $ cN ++ suffix) g
      t = addChild (make $ tN ++ suffix) c
      r = addChild (make $ rN ++ suffix) t
      rt = addChild (make (rtN ++ suffix) :: Root) r
      trRN = getName emptyTrainRunRepo in 
  [[]]  == (nub $ [find (one rN) rt]) &&
  [[]]  == (nub $ [find (one tN) r]    ++ [find (two rN tN) rt]) && 
  [[]]  == (nub $ [find (one cN) t]    ++ [find (two tN cN) r]   ++ [find (three rN tN cN) rt]) &&
  [[]]  == (nub $ [find (one gN) c]    ++ [find (two cN gN) t]   ++ [find (three tN cN gN) r]    ++ [find (four rN tN cN gN) rt]) &&
  [[]]  == (nub $ [find (one pN) g]    ++ [find (two gN pN) c]   ++ [find (three cN gN pN) t]    ++ [find (four tN cN gN pN) r]    ++ [find (five rN tN cN gN pN) rt]) &&
  [[]]  == (nub $ [find (one trN) trr] ++ [find (two pN trRN) p] ++ [find (three pN trRN trN) g] ++ [find (four gN pN trRN trN) c] ++ [find (five cN gN pN trRN trN) t]  ++ [find (six rN cN gN pN trRN trN) r] ++ [find (seven rN tN cN gN pN trRN trN) rt])

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


