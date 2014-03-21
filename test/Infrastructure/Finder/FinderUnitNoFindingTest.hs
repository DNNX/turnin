{-# OPTIONS_GHC -F -pgmF htfpp -fno-warn-incomplete-patterns#-}
module Infrastructure.Finder.FinderUnitNoFindingTest where

import Test.Framework
import Infrastructure.Finder.FinderTestUtils

import Infrastructure.Node
import Domain.Root
import Domain.Project
import Domain.ProjectRepo

{-# ANN module "HLint: ignore Use camelCase" #-}
{-# ANN module "HLint: ignore Reduce duplication" #-}

zip' [] [] = []
zip' [] _  = error "Lists of unequal lengths"
zip' _  [] = error "Lists of unequal lenghts"
zip' (x:xs) (y:ys) = (x,y):zip' xs ys

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
      [p1,p2] = map (setTrainRunRepo p0) [trr,trr1]
      trr = emptyTrainRunRepo
      [trr1] = map (addChild trr) [tr0]
      tr0 = make trN

      pTrr = makeProjectTrainRunRepo trr

  assertState (one' find' rt0   ldRoot ldR)  rootC []
  assertState (one' find' r0    ldR    ldT)  rC    []
  assertState (one' find' t0    ldT    ldC)  tC    []
  assertState (one' find' c0    ldC    ldG)  cC    []
  assertState (one' find' g0    ldG    ldP)  gC    []
  assertState (one' find' pTrr  ldPr   ldTr) prC   []

  mapM_(\(n,call)->assertState (two' find' n ldRoot ldR  ldT)  call []) $ zip' [rt0,rt1] [rootC,rootC|+rC]
  mapM_(\(n,call)->assertState (two' find' n ldR    ldT  ldC)  call []) $ zip' [r0,r1]   [rC,rC|+tC]
  mapM_(\(n,call)->assertState (two' find' n ldT    ldC  ldG)  call []) $ zip' [t0,t1]   [tC,tC|+cC]
  mapM_(\(n,call)->assertState (two' find' n ldC    ldG  ldP)  call []) $ zip' [c0,c1]   [cC,cC|+gC]
  mapM_(\(n,call)->assertState (two' find' n ldG    ldP  ldPr) call []) $ zip' [g0]      [gC]
  mapM_(\(n,call)->assertState (two' find' n ldP    ldPr ldTr) call []) $ zip' [p0,p1]   [pC|+pr3C,pC|+pr3C]

  mapM_(\(n,call)->assertState (three' find' n ldRoot ldR ldT  ldC)  call []) $ zip' [rt0,rt1,rt2] [rootC,rootC|+rC,rootC|+rC|+tC]
  mapM_(\(n,call)->assertState (three' find' n ldR    ldT ldC  ldG)  call []) $ zip' [r0,r1,r2]    [rC,rC|+tC,rC|+tC|+cC]
  mapM_(\(n,call)->assertState (three' find' n ldT    ldC ldG  ldP)  call []) $ zip' [t0,t1,t2]    [tC,tC|+cC,tC|+cC|+gC]
  mapM_(\(n,call)->assertState (three' find' n ldG    ldP ldPr ldTr) call []) $ zip' [g0,g1,g2]    [gC,gC|+pC|+pr3C,gC|+pC|+pr3C]

  mapM_(\(n,call)->assertState (four' find' n ldRoot ldR ldT ldC  ldG)  call []) $ zip' [rt0,rt1,rt2,rt3] [rootC,rootC|+rC,rootC|+rC|+tC,rootC|+rC|+tC|+cC]
  mapM_(\(n,call)->assertState (four' find' n ldR    ldT ldC ldG  ldP)  call []) $ zip' [r0,r1,r2,r3]     [rC,rC|+tC,rC|+tC|+cC,rC|+tC|+cC|+gC]
  mapM_(\(n,call)->assertState (four' find' n ldT    ldC ldG ldP  ldPr) call []) $ zip' [t0,t1,t2]        [tC,tC|+cC,tC|+cC|+gC]
  mapM_(\(n,call)->assertState (four' find' n ldC    ldG ldP ldPr ldTr) call []) $ zip' [c0,c1,c2,c3]     [cC,cC|+gC,cC|+gC|+pC|+pr3C,cC|+gC|+pC|+pr3C]

  mapM_(\(n,call)->assertState (five' find' n ldRoot ldR ldT ldC  ldG  ldP)  call []) $ zip' [rt0,rt1,rt2,rt3,rt4] [rootC,rootC|+rC,rootC|+rC|+tC,rootC|+rC|+tC|+cC,rootC|+rC|+tC|+cC|+gC]
  mapM_(\(n,call)->assertState (five' find' n ldR    ldT ldC ldG  ldP  ldPr) call []) $ zip' [r0,r1,r2,r3]         [rC,rC|+tC,rC|+tC|+cC,rC|+tC|+cC|+gC]
  mapM_(\(n,call)->assertState (five' find' n ldT    ldC ldG ldP  ldPr ldTr) call []) $ zip' [t0,t1,t2,t3,t4]      [tC,tC|+cC,tC|+cC|+gC,tC|+cC|+gC|+pC|+pr3C,tC|+cC|+gC|+pC|+pr3C]

  mapM_(\(n,call)->assertState (six' find' n ldRoot ldR ldT ldC ldG  ldP  ldPr) call []) $ zip' [rt0,rt1,rt2,rt3,rt4] [rootC,rootC|+rC,rootC|+rC|+tC,rootC|+rC|+tC|+cC,rootC|+rC|+tC|+cC|+gC]
  mapM_(\(n,call)->assertState (six' find' n ldR    ldT ldC ldG ldP  ldPr ldTr) call []) $ zip' [r0,r1,r2,r3,r4,r5]   [rC,rC|+tC,rC|+tC|+cC,rC|+tC|+cC|+gC,rC|+tC|+cC|+gC|+pC|+pr3C,rC|+tC|+cC|+gC|+pC|+pr3C]

  mapM_(\(n,call)->assertState (seven' find' n ldRoot ldR ldT ldC ldG ldP ldPr ldTr) call []) $ zip' [rt0,rt1,rt2,rt3,rt4,rt5,rt6] [rootC,rootC|+rC,rootC|+rC|+tC,rootC|+rC|+tC|+cC,rootC|+rC|+tC|+cC|+gC,rootC|+rC|+tC|+cC|+gC|+pC|+pr3C,rootC|+rC|+tC|+cC|+gC|+pC|+pr3C]

  assertState (one' findUnambiguous' rt0   ldRoot ldR)  rootC Nothing
  assertState (one' findUnambiguous' r0    ldR    ldT)  rC    Nothing
  assertState (one' findUnambiguous' t0    ldT    ldC)  tC    Nothing
  assertState (one' findUnambiguous' c0    ldC    ldG)  cC    Nothing
  assertState (one' findUnambiguous' g0    ldG    ldP)  gC    Nothing
  assertState (one' findUnambiguous' pTrr  ldPr   ldTr) prC   Nothing

  mapM_(\(n,call)->assertState (two' findUnambiguous' n ldRoot ldR  ldT)  call Nothing) $ zip' [rt0,rt1] [rootC,rootC|+rC]
  mapM_(\(n,call)->assertState (two' findUnambiguous' n ldR    ldT  ldC)  call Nothing) $ zip' [r0,r1]   [rC,rC|+tC]
  mapM_(\(n,call)->assertState (two' findUnambiguous' n ldT    ldC  ldG)  call Nothing) $ zip' [t0,t1]   [tC,tC|+cC]
  mapM_(\(n,call)->assertState (two' findUnambiguous' n ldC    ldG  ldP)  call Nothing) $ zip' [c0,c1]   [cC,cC|+gC]
  mapM_(\(n,call)->assertState (two' findUnambiguous' n ldG    ldP  ldPr) call Nothing) $ zip' [g0]      [gC]
  mapM_(\(n,call)->assertState (two' findUnambiguous' n ldP    ldPr ldTr) call Nothing) $ zip' [p0,p1]   [pC|+pr3C,pC|+pr3C]

  mapM_(\(n,call)->assertState (three' findUnambiguous' n ldRoot ldR ldT  ldC)  call Nothing) $ zip' [rt0,rt1,rt2] [rootC,rootC|+rC,rootC|+rC|+tC]
  mapM_(\(n,call)->assertState (three' findUnambiguous' n ldR    ldT ldC  ldG)  call Nothing) $ zip' [r0,r1,r2]    [rC,rC|+tC,rC|+tC|+cC]
  mapM_(\(n,call)->assertState (three' findUnambiguous' n ldT    ldC ldG  ldP)  call Nothing) $ zip' [t0,t1,t2]    [tC,tC|+cC,tC|+cC|+gC]
  mapM_(\(n,call)->assertState (three' findUnambiguous' n ldG    ldP ldPr ldTr) call Nothing) $ zip' [g0,g1,g2]    [gC,gC|+pC|+pr3C,gC|+pC|+pr3C]

  mapM_(\(n,call)->assertState (four' findUnambiguous' n ldRoot ldR ldT ldC  ldG)  call Nothing) $ zip' [rt0,rt1,rt2,rt3] [rootC,rootC|+rC,rootC|+rC|+tC,rootC|+rC|+tC|+cC]
  mapM_(\(n,call)->assertState (four' findUnambiguous' n ldR    ldT ldC ldG  ldP)  call Nothing) $ zip' [r0,r1,r2,r3]     [rC,rC|+tC,rC|+tC|+cC,rC|+tC|+cC|+gC]
  mapM_(\(n,call)->assertState (four' findUnambiguous' n ldT    ldC ldG ldP  ldPr) call Nothing) $ zip' [t0,t1,t2]        [tC,tC|+cC,tC|+cC|+gC]
  mapM_(\(n,call)->assertState (four' findUnambiguous' n ldC    ldG ldP ldPr ldTr) call Nothing) $ zip' [c0,c1,c2,c3]     [cC,cC|+gC,cC|+gC|+pC|+pr3C,cC|+gC|+pC|+pr3C]

  mapM_(\(n,call)->assertState (five' findUnambiguous' n ldRoot ldR ldT ldC  ldG  ldP)  call Nothing) $ zip' [rt0,rt1,rt2,rt3,rt4] [rootC,rootC|+rC,rootC|+rC|+tC,rootC|+rC|+tC|+cC,rootC|+rC|+tC|+cC|+gC]
  mapM_(\(n,call)->assertState (five' findUnambiguous' n ldR    ldT ldC ldG  ldP  ldPr) call Nothing) $ zip' [r0,r1,r2,r3]         [rC,rC|+tC,rC|+tC|+cC,rC|+tC|+cC|+gC]
  mapM_(\(n,call)->assertState (five' findUnambiguous' n ldT    ldC ldG ldP  ldPr ldTr) call Nothing) $ zip' [t0,t1,t2,t3,t4]      [tC,tC|+cC,tC|+cC|+gC,tC|+cC|+gC|+pC|+pr3C,tC|+cC|+gC|+pC|+pr3C]

  mapM_(\(n,call)->assertState (six' findUnambiguous' n ldRoot ldR ldT ldC ldG  ldP  ldPr) call Nothing) $ zip' [rt0,rt1,rt2,rt3,rt4] [rootC,rootC|+rC,rootC|+rC|+tC,rootC|+rC|+tC|+cC,rootC|+rC|+tC|+cC|+gC]
  mapM_(\(n,call)->assertState (six' findUnambiguous' n ldR    ldT ldC ldG ldP  ldPr ldTr) call Nothing) $ zip' [r0,r1,r2,r3,r4,r5]   [rC,rC|+tC,rC|+tC|+cC,rC|+tC|+cC|+gC,rC|+tC|+cC|+gC|+pC|+pr3C,rC|+tC|+cC|+gC|+pC|+pr3C]

  mapM_(\(n,call)->assertState (seven' findUnambiguous' n ldRoot ldR ldT ldC ldG ldP ldPr ldTr) call Nothing) $ zip' [rt0,rt1,rt2,rt3,rt4,rt5,rt6] [rootC,rootC|+rC,rootC|+rC|+tC,rootC|+rC|+tC|+cC,rootC|+rC|+tC|+cC|+gC,rootC|+rC|+tC|+cC|+gC|+pC|+pr3C,rootC|+rC|+tC|+cC|+gC|+pC|+pr3C]

test_noFindAllGoodHints = do
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
      [p1,p2] = map (setTrainRunRepo p0) [trr,trr1]
      trr = emptyTrainRunRepo
      [trr1] = map (addChild trr) [tr0]
      tr0 = make trN

      pTrr = makeProjectTrainRunRepo trr
      trRN = getName trr

  assertState (one rN   find' rt0   ldRoot ldR)  rootC []
  assertState (one tN   find' r0    ldR    ldT)  rC    []
  assertState (one cN   find' t0    ldT    ldC)  tC    []
  assertState (one gN   find' c0    ldC    ldG)  cC    []
  assertState (one pN   find' g0    ldG    ldP)  gC    []
  assertState (one trRN find' pTrr  ldPr   ldTr) prC   []

  mapM_(\(n,call)->assertState (two rN   tN   find' n ldRoot ldR  ldT)  call []) $ zip' [rt0,rt1] [rootC,rootC|+rC]
  mapM_(\(n,call)->assertState (two tN   cN   find' n ldR    ldT  ldC)  call []) $ zip' [r0,r1]   [rC,rC|+tC]
  mapM_(\(n,call)->assertState (two cN   gN   find' n ldT    ldC  ldG)  call []) $ zip' [t0,t1]   [tC,tC|+cC]
  mapM_(\(n,call)->assertState (two gN   pN   find' n ldC    ldG  ldP)  call []) $ zip' [c0,c1]   [cC,cC|+gC]
  mapM_(\(n,call)->assertState (two pN   trRN find' n ldG    ldP  ldPr) call []) $ zip' [g0]      [gC]
  mapM_(\(n,call)->assertState (two trRN trN find' n ldP    ldPr ldTr)  call []) $ zip' [p0,p1]   [pC|+prC,pC|+prC]

  mapM_(\(n,call)->assertState (three rN  tN  cN  find' n ldRoot ldR ldT  ldC)  call []) $ zip' [rt0,rt1,rt2] [rootC,rootC|+rC,rootC|+rC|+tC]
  mapM_(\(n,call)->assertState (three tN  cN  gN  find' n ldR    ldT ldC  ldG)  call []) $ zip' [r0,r1,r2]    [rC,rC|+tC,rC|+tC|+cC]
  mapM_(\(n,call)->assertState (three cN  gN  pN  find' n ldT    ldC ldG  ldP)  call []) $ zip' [t0,t1,t2]    [tC,tC|+cC,tC|+cC|+gC]
  mapM_(\(n,call)->assertState (three pN trRN trN find' n ldG    ldP ldPr ldTr) call []) $ zip' [g0,g1,g2]    [gC,gC|+pC|+prC,gC|+pC|+prC]

  mapM_(\(n,call)->assertState (four rN tN cN   gN   find' n ldRoot ldR ldT ldC  ldG)  call []) $ zip' [rt0,rt1,rt2,rt3] [rootC,rootC|+rC,rootC|+rC|+tC,rootC|+rC|+tC|+cC]
  mapM_(\(n,call)->assertState (four tN cN gN   pN   find' n ldR    ldT ldC ldG  ldP)  call []) $ zip' [r0,r1,r2,r3]     [rC,rC|+tC,rC|+tC|+cC,rC|+tC|+cC|+gC]
  mapM_(\(n,call)->assertState (four cN gN pN   trRN find' n ldT    ldC ldG ldP  ldPr) call []) $ zip' [t0,t1,t2]        [tC,tC|+cC,tC|+cC|+gC]
  mapM_(\(n,call)->assertState (four gN pN trRN trN  find' n ldC    ldG ldP ldPr ldTr) call []) $ zip' [c0,c1,c2,c3]     [cC,cC|+gC,cC|+gC|+pC|+prC,cC|+gC|+pC|+prC]

  mapM_(\(n,call)->assertState (five rN tN cN gN   pN   find' n ldRoot ldR ldT ldC  ldG  ldP)  call []) $ zip' [rt0,rt1,rt2,rt3,rt4] [rootC,rootC|+rC,rootC|+rC|+tC,rootC|+rC|+tC|+cC,rootC|+rC|+tC|+cC|+gC]
  mapM_(\(n,call)->assertState (five tN cN gN pN   trRN find' n ldR    ldT ldC ldG  ldP  ldPr) call []) $ zip' [r0,r1,r2,r3]         [rC,rC|+tC,rC|+tC|+cC,rC|+tC|+cC|+gC]
  mapM_(\(n,call)->assertState (five cN gN pN trRN trN  find' n ldT    ldC ldG ldP  ldPr ldTr) call []) $ zip' [t0,t1,t2,t3,t4]      [tC,tC|+cC,tC|+cC|+gC,tC|+cC|+gC|+pC|+prC,tC|+cC|+gC|+pC|+prC]

  mapM_(\(n,call)->assertState (six rN tN cN gN pN   trRN find' n ldRoot ldR ldT ldC ldG  ldP  ldPr) call []) $ zip' [rt0,rt1,rt2,rt3,rt4] [rootC,rootC|+rC,rootC|+rC|+tC,rootC|+rC|+tC|+cC,rootC|+rC|+tC|+cC|+gC]
  mapM_(\(n,call)->assertState (six tN cN gN pN trRN trN  find' n ldR    ldT ldC ldG ldP  ldPr ldTr) call []) $ zip' [r0,r1,r2,r3,r4,r5]   [rC,rC|+tC,rC|+tC|+cC,rC|+tC|+cC|+gC,rC|+tC|+cC|+gC|+pC|+prC,rC|+tC|+cC|+gC|+pC|+prC]

  mapM_(\(n,call)->assertState (seven rN tN cN gN pN trRN trN find' n ldRoot ldR ldT ldC ldG ldP ldPr ldTr) call []) $ zip' [rt0,rt1,rt2,rt3,rt4,rt5,rt6] [rootC,rootC|+rC,rootC|+rC|+tC,rootC|+rC|+tC|+cC,rootC|+rC|+tC|+cC|+gC,rootC|+rC|+tC|+cC|+gC|+pC|+prC,rootC|+rC|+tC|+cC|+gC|+pC|+prC]

  assertState (one rN   findUnambiguous' rt0   ldRoot ldR)  rootC Nothing
  assertState (one tN   findUnambiguous' r0    ldR    ldT)  rC    Nothing
  assertState (one cN   findUnambiguous' t0    ldT    ldC)  tC    Nothing
  assertState (one gN   findUnambiguous' c0    ldC    ldG)  cC    Nothing
  assertState (one pN   findUnambiguous' g0    ldG    ldP)  gC    Nothing
  assertState (one trRN findUnambiguous' pTrr  ldPr   ldTr) prC   Nothing

  mapM_(\(n,call)->assertState (two rN   tN   findUnambiguous' n ldRoot ldR  ldT)  call Nothing) $ zip' [rt0,rt1] [rootC,rootC|+rC]
  mapM_(\(n,call)->assertState (two tN   cN   findUnambiguous' n ldR    ldT  ldC)  call Nothing) $ zip' [r0,r1]   [rC,rC|+tC]
  mapM_(\(n,call)->assertState (two cN   gN   findUnambiguous' n ldT    ldC  ldG)  call Nothing) $ zip' [t0,t1]   [tC,tC|+cC]
  mapM_(\(n,call)->assertState (two gN   pN   findUnambiguous' n ldC    ldG  ldP)  call Nothing) $ zip' [c0,c1]   [cC,cC|+gC]
  mapM_(\(n,call)->assertState (two pN   trRN findUnambiguous' n ldG    ldP  ldPr) call Nothing) $ zip' [g0]      [gC]
  mapM_(\(n,call)->assertState (two trRN trN findUnambiguous' n ldP    ldPr ldTr)  call Nothing) $ zip' [p0,p1]   [pC|+prC,pC|+prC]

  mapM_(\(n,call)->assertState (three rN  tN  cN  findUnambiguous' n ldRoot ldR ldT  ldC)  call Nothing) $ zip' [rt0,rt1,rt2] [rootC,rootC|+rC,rootC|+rC|+tC]
  mapM_(\(n,call)->assertState (three tN  cN  gN  findUnambiguous' n ldR    ldT ldC  ldG)  call Nothing) $ zip' [r0,r1,r2]    [rC,rC|+tC,rC|+tC|+cC]
  mapM_(\(n,call)->assertState (three cN  gN  pN  findUnambiguous' n ldT    ldC ldG  ldP)  call Nothing) $ zip' [t0,t1,t2]    [tC,tC|+cC,tC|+cC|+gC]
  mapM_(\(n,call)->assertState (three pN trRN trN findUnambiguous' n ldG    ldP ldPr ldTr) call Nothing) $ zip' [g0,g1,g2]    [gC,gC|+pC|+prC,gC|+pC|+prC]

  mapM_(\(n,call)->assertState (four rN tN cN   gN   findUnambiguous' n ldRoot ldR ldT ldC  ldG)  call Nothing) $ zip' [rt0,rt1,rt2,rt3] [rootC,rootC|+rC,rootC|+rC|+tC,rootC|+rC|+tC|+cC]
  mapM_(\(n,call)->assertState (four tN cN gN   pN   findUnambiguous' n ldR    ldT ldC ldG  ldP)  call Nothing) $ zip' [r0,r1,r2,r3]     [rC,rC|+tC,rC|+tC|+cC,rC|+tC|+cC|+gC]
  mapM_(\(n,call)->assertState (four cN gN pN   trRN findUnambiguous' n ldT    ldC ldG ldP  ldPr) call Nothing) $ zip' [t0,t1,t2]        [tC,tC|+cC,tC|+cC|+gC]
  mapM_(\(n,call)->assertState (four gN pN trRN trN  findUnambiguous' n ldC    ldG ldP ldPr ldTr) call Nothing) $ zip' [c0,c1,c2,c3]     [cC,cC|+gC,cC|+gC|+pC|+prC,cC|+gC|+pC|+prC]

  mapM_(\(n,call)->assertState (five rN tN cN gN   pN   findUnambiguous' n ldRoot ldR ldT ldC  ldG  ldP)  call Nothing) $ zip' [rt0,rt1,rt2,rt3,rt4] [rootC,rootC|+rC,rootC|+rC|+tC,rootC|+rC|+tC|+cC,rootC|+rC|+tC|+cC|+gC]
  mapM_(\(n,call)->assertState (five tN cN gN pN   trRN findUnambiguous' n ldR    ldT ldC ldG  ldP  ldPr) call Nothing) $ zip' [r0,r1,r2,r3]         [rC,rC|+tC,rC|+tC|+cC,rC|+tC|+cC|+gC]
  mapM_(\(n,call)->assertState (five cN gN pN trRN trN  findUnambiguous' n ldT    ldC ldG ldP  ldPr ldTr) call Nothing) $ zip' [t0,t1,t2,t3,t4]      [tC,tC|+cC,tC|+cC|+gC,tC|+cC|+gC|+pC|+prC,tC|+cC|+gC|+pC|+prC]

  mapM_(\(n,call)->assertState (six rN tN cN gN pN   trRN findUnambiguous' n ldRoot ldR ldT ldC ldG  ldP  ldPr) call Nothing) $ zip' [rt0,rt1,rt2,rt3,rt4] [rootC,rootC|+rC,rootC|+rC|+tC,rootC|+rC|+tC|+cC,rootC|+rC|+tC|+cC|+gC]
  mapM_(\(n,call)->assertState (six tN cN gN pN trRN trN  findUnambiguous' n ldR    ldT ldC ldG ldP  ldPr ldTr) call Nothing) $ zip' [r0,r1,r2,r3,r4,r5]   [rC,rC|+tC,rC|+tC|+cC,rC|+tC|+cC|+gC,rC|+tC|+cC|+gC|+pC|+prC,rC|+tC|+cC|+gC|+pC|+prC]

  mapM_(\(n,call)->assertState (seven rN tN cN gN pN trRN trN findUnambiguous' n ldRoot ldR ldT ldC ldG ldP ldPr ldTr) call Nothing) $ zip' [rt0,rt1,rt2,rt3,rt4,rt5,rt6] [rootC,rootC|+rC,rootC|+rC|+tC,rootC|+rC|+tC|+cC,rootC|+rC|+tC|+cC|+gC,rootC|+rC|+tC|+cC|+gC|+pC|+prC,rootC|+rC|+tC|+cC|+gC|+pC|+prC]

test_noFindAllBadHints = do
  let suffix = "0"
      tr = make $ trN ++ suffix
      trr = addChild emptyTrainRunRepo tr
      p = setTrainRunRepo (make $ pN ++ suffix) trr
      g = addChild (make $ gN ++ suffix) p
      c = addChild (make $ cN ++ suffix) g
      t = addChild (make $ tN ++ suffix) c
      r = addChild (make $ rN ++ suffix) t
      root = addChild (make (rootN ++ suffix) :: Root) r

      rootN = "root"
      rN = "repo"
      tN = "term"
      cN = "course"
      gN = "group"
      pN = "project"
      trN = "trainRun"

      sRN = getName emptySubmitRepo ++ suffix
      tfRN = getName emptyTrainFileRepo ++ suffix
      trRN = getName emptyTrainRunRepo ++ suffix

      pTrr = makeProjectTrainRunRepo trr

  assertState (one rN   find' root ldRoot ldR)  rootC []
  assertState (one tN   find' r    ldR    ldT)  rC    []
  assertState (one cN   find' t    ldT    ldC)  tC    []
  assertState (one gN   find' c    ldC    ldG)  cC    []
  assertState (one pN   find' g    ldG    ldP)  gC    []
  assertState (one sRN  find' p    ldP    ldPr) pC    []
  assertState (one tfRN find' p    ldP    ldPr) pC    []
  assertState (one trRN find' p    ldP    ldPr) pC    []
  assertState (one trN  find' pTrr ldPr   ldTr) prC   []

  assertState (one rN   findUnambiguous' root ldRoot ldR)  rootC Nothing
  assertState (one tN   findUnambiguous' r    ldR    ldT)  rC    Nothing
  assertState (one cN   findUnambiguous' t    ldT    ldC)  tC    Nothing
  assertState (one gN   findUnambiguous' c    ldC    ldG)  cC    Nothing
  assertState (one pN   findUnambiguous' g    ldG    ldP)  gC    Nothing
  assertState (one sRN  findUnambiguous' p    ldP    ldPr) pC    Nothing
  assertState (one tfRN findUnambiguous' p    ldP    ldPr) pC    Nothing
  assertState (one trRN findUnambiguous' p    ldP    ldPr) pC    Nothing
  assertState (one trN  findUnambiguous' pTrr ldPr   ldTr) prC   Nothing

  assertState (two rN   tN   find' root ldRoot ldR  ldT)  rootC []
  assertState (two tN   cN   find' r    ldR    ldT  ldC)  rC    []
  assertState (two cN   gN   find' t    ldT    ldC  ldG)  tC    []
  assertState (two gN   pN   find' c    ldC    ldG  ldP)  cC    []
  assertState (two pN   sRN  find' g    ldG    ldP  ldPr) gC    []
  assertState (two pN   tfRN find' g    ldG    ldP  ldPr) gC    []
  assertState (two pN   trRN find' g    ldG    ldP  ldPr) gC    []
  assertState (two trRN trN  find' p    ldP    ldPr ldTr) pC    []

  assertState (two rN   tN   findUnambiguous' root ldRoot ldR  ldT)  rootC Nothing
  assertState (two tN   cN   findUnambiguous' r    ldR    ldT  ldC)  rC    Nothing
  assertState (two cN   gN   findUnambiguous' t    ldT    ldC  ldG)  tC    Nothing
  assertState (two gN   pN   findUnambiguous' c    ldC    ldG  ldP)  cC    Nothing
  assertState (two pN   sRN  findUnambiguous' g    ldG    ldP  ldPr) gC    Nothing
  assertState (two pN   tfRN findUnambiguous' g    ldG    ldP  ldPr) gC    Nothing
  assertState (two pN   trRN findUnambiguous' g    ldG    ldP  ldPr) gC    Nothing
  assertState (two trRN trN  findUnambiguous' p    ldP    ldPr ldTr) pC    Nothing

  assertState (three rN tN   cN   find' root ldRoot ldR ldT  ldC)  rootC []
  assertState (three tN cN   gN   find' r    ldR    ldT ldC  ldG)  rC    []
  assertState (three cN gN   pN   find' t    ldT    ldC ldG  ldP)  tC    []
  assertState (three gN pN   sRN  find' c    ldC    ldG ldP  ldPr) cC    []
  assertState (three gN pN   tfRN find' c    ldC    ldG ldP  ldPr) cC    []
  assertState (three gN pN   trRN find' c    ldC    ldG ldP  ldPr) cC    []
  assertState (three pN trRN trN  find' g    ldG    ldP ldPr ldTr) gC    []

  assertState (three rN tN   cN   findUnambiguous' root ldRoot ldR ldT  ldC)  rootC Nothing
  assertState (three tN cN   gN   findUnambiguous' r    ldR    ldT ldC  ldG)  rC    Nothing
  assertState (three cN gN   pN   findUnambiguous' t    ldT    ldC ldG  ldP)  tC    Nothing
  assertState (three gN pN   sRN  findUnambiguous' c    ldC    ldG ldP  ldPr) cC    Nothing
  assertState (three gN pN   tfRN findUnambiguous' c    ldC    ldG ldP  ldPr) cC    Nothing
  assertState (three gN pN   trRN findUnambiguous' c    ldC    ldG ldP  ldPr) cC    Nothing
  assertState (three pN trRN trN  findUnambiguous' g    ldG    ldP ldPr ldTr) gC    Nothing

  assertState (four rN tN cN   gN   find' root ldRoot ldR ldT ldC  ldG)  rootC []
  assertState (four tN cN gN   pN   find' r    ldR    ldT ldC ldG  ldP)  rC    []
  assertState (four cN gN pN   sRN  find' t    ldT    ldC ldG ldP  ldPr) tC    []
  assertState (four cN gN pN   tfRN find' t    ldT    ldC ldG ldP  ldPr) tC    []
  assertState (four cN gN pN   trRN find' t    ldT    ldC ldG ldP  ldPr) tC    []
  assertState (four gN pN trRN trN  find' c    ldC    ldG ldP ldPr ldTr) cC    []

  assertState (four rN tN cN   gN   findUnambiguous' root ldRoot ldR ldT ldC  ldG)  rootC Nothing
  assertState (four tN cN gN   pN   findUnambiguous' r    ldR    ldT ldC ldG  ldP)  rC    Nothing
  assertState (four cN gN pN   sRN  findUnambiguous' t    ldT    ldC ldG ldP  ldPr) tC    Nothing
  assertState (four cN gN pN   tfRN findUnambiguous' t    ldT    ldC ldG ldP  ldPr) tC    Nothing
  assertState (four cN gN pN   trRN findUnambiguous' t    ldT    ldC ldG ldP  ldPr) tC    Nothing
  assertState (four gN pN trRN trN  findUnambiguous' c    ldC    ldG ldP ldPr ldTr) cC    Nothing

  assertState (five rN tN cN gN   pN   find' root ldRoot ldR ldT ldC ldG  ldP)  rootC []
  assertState (five tN cN gN pN   sRN  find' r    ldR    ldT ldC ldG ldP  ldPr) rC    []
  assertState (five tN cN gN pN   tfRN find' r    ldR    ldT ldC ldG ldP  ldPr) rC    []
  assertState (five tN cN gN pN   trRN find' r    ldR    ldT ldC ldG ldP  ldPr) rC    []
  assertState (five cN gN pN trRN trN  find' t    ldT    ldC ldG ldP ldPr ldTr) tC    []

  assertState (five rN tN cN gN   pN   findUnambiguous' root ldRoot ldR ldT ldC ldG  ldP)  rootC  Nothing
  assertState (five tN cN gN pN   sRN  findUnambiguous' r    ldR    ldT ldC ldG ldP  ldPr) rC     Nothing
  assertState (five tN cN gN pN   tfRN findUnambiguous' r    ldR    ldT ldC ldG ldP  ldPr) rC     Nothing
  assertState (five tN cN gN pN   trRN findUnambiguous' r    ldR    ldT ldC ldG ldP  ldPr) rC     Nothing
  assertState (five cN gN pN trRN trN  findUnambiguous' t    ldT    ldC ldG ldP ldPr ldTr) tC     Nothing

  assertState (six rN tN cN gN pN   sRN  find' root ldRoot ldR ldT ldC ldG ldP  ldPr) rootC []
  assertState (six rN tN cN gN pN   tfRN find' root ldRoot ldR ldT ldC ldG ldP  ldPr) rootC []
  assertState (six rN tN cN gN pN   trRN find' root ldRoot ldR ldT ldC ldG ldP  ldPr) rootC []
  assertState (six tN cN gN pN trRN trN  find' r    ldR    ldT ldC ldG ldP ldPr ldTr) rC    []

  assertState (six rN tN cN gN pN   sRN  findUnambiguous' root ldRoot ldR ldT ldC ldG ldP  ldPr) rootC Nothing
  assertState (six rN tN cN gN pN   tfRN findUnambiguous' root ldRoot ldR ldT ldC ldG ldP  ldPr) rootC Nothing
  assertState (six rN tN cN gN pN   trRN findUnambiguous' root ldRoot ldR ldT ldC ldG ldP  ldPr) rootC Nothing
  assertState (six tN cN gN pN trRN trN  findUnambiguous' r    ldR    ldT ldC ldG ldP ldPr ldTr) rC    Nothing

  assertState (seven rN tN cN gN pN trRN trN find'            root ldRoot ldR ldT ldC ldG ldP ldPr ldTr) rootC  []
  assertState (seven rN tN cN gN pN trRN trN findUnambiguous' root ldRoot ldR ldT ldC ldG ldP ldPr ldTr) rootC  Nothing
