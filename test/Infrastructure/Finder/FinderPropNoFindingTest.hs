{-# OPTIONS_GHC -F -pgmF htfpp -fno-warn-incomplete-patterns#-}
module Infrastructure.Finder.FinderPropNoFindingTest where

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

prop_noFindNoHints rootN rN tN cN gN pN trN = 
 "" `notElem` [rootN,rN,tN,cN,gN,pN,trN] ==>
  let rt0 = make rootN :: Root
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
  in assertStateP (one' find' rt0   ldRoot ldR)  rootC []
  && assertStateP (one' find' r0    ldR    ldT)  rC    []
  && assertStateP (one' find' t0    ldT    ldC)  tC    []
  && assertStateP (one' find' c0    ldC    ldG)  cC    []
  && assertStateP (one' find' g0    ldG    ldP)  gC    []
  && assertStateP (one' find' pTrr  ldPr   ldTr) prC   []

  && all(\(n,call)->assertStateP (two' find' n ldRoot ldR  ldT)  call []) (zip' [rt0,rt1] [rootC,rootC|+rC])
  && all(\(n,call)->assertStateP (two' find' n ldR    ldT  ldC)  call []) (zip' [r0,r1]   [rC,rC|+tC])
  && all(\(n,call)->assertStateP (two' find' n ldT    ldC  ldG)  call []) (zip' [t0,t1]   [tC,tC|+cC])
  && all(\(n,call)->assertStateP (two' find' n ldC    ldG  ldP)  call []) (zip' [c0,c1]   [cC,cC|+gC])
  && all(\(n,call)->assertStateP (two' find' n ldG    ldP  ldPr) call []) (zip' [g0]      [gC])
  && all(\(n,call)->assertStateP (two' find' n ldP    ldPr ldTr) call []) (zip' [p0,p1]   [pC|+pr3C,pC|+pr3C])

  && all(\(n,call)->assertStateP (three' find' n ldRoot ldR ldT  ldC)  call []) (zip' [rt0,rt1,rt2] [rootC,rootC|+rC,rootC|+rC|+tC])
  && all(\(n,call)->assertStateP (three' find' n ldR    ldT ldC  ldG)  call []) (zip' [r0,r1,r2]    [rC,rC|+tC,rC|+tC|+cC])
  && all(\(n,call)->assertStateP (three' find' n ldT    ldC ldG  ldP)  call []) (zip' [t0,t1,t2]    [tC,tC|+cC,tC|+cC|+gC])
  && all(\(n,call)->assertStateP (three' find' n ldG    ldP ldPr ldTr) call []) (zip' [g0,g1,g2]    [gC,gC|+pC|+pr3C,gC|+pC|+pr3C])

  && all(\(n,call)->assertStateP (four' find' n ldRoot ldR ldT ldC  ldG)  call []) (zip' [rt0,rt1,rt2,rt3] [rootC,rootC|+rC,rootC|+rC|+tC,rootC|+rC|+tC|+cC])
  && all(\(n,call)->assertStateP (four' find' n ldR    ldT ldC ldG  ldP)  call []) (zip' [r0,r1,r2,r3]     [rC,rC|+tC,rC|+tC|+cC,rC|+tC|+cC|+gC])
  && all(\(n,call)->assertStateP (four' find' n ldT    ldC ldG ldP  ldPr) call []) (zip' [t0,t1,t2]        [tC,tC|+cC,tC|+cC|+gC])
  && all(\(n,call)->assertStateP (four' find' n ldC    ldG ldP ldPr ldTr) call []) (zip' [c0,c1,c2,c3]     [cC,cC|+gC,cC|+gC|+pC|+pr3C,cC|+gC|+pC|+pr3C])

  && all(\(n,call)->assertStateP (five' find' n ldRoot ldR ldT ldC  ldG  ldP)  call []) (zip' [rt0,rt1,rt2,rt3,rt4] [rootC,rootC|+rC,rootC|+rC|+tC,rootC|+rC|+tC|+cC,rootC|+rC|+tC|+cC|+gC])
  && all(\(n,call)->assertStateP (five' find' n ldR    ldT ldC ldG  ldP  ldPr) call []) (zip' [r0,r1,r2,r3]         [rC,rC|+tC,rC|+tC|+cC,rC|+tC|+cC|+gC])
  && all(\(n,call)->assertStateP (five' find' n ldT    ldC ldG ldP  ldPr ldTr) call []) (zip' [t0,t1,t2,t3,t4]      [tC,tC|+cC,tC|+cC|+gC,tC|+cC|+gC|+pC|+pr3C,tC|+cC|+gC|+pC|+pr3C])

  && all(\(n,call)->assertStateP (six' find' n ldRoot ldR ldT ldC ldG  ldP  ldPr) call []) (zip' [rt0,rt1,rt2,rt3,rt4] [rootC,rootC|+rC,rootC|+rC|+tC,rootC|+rC|+tC|+cC,rootC|+rC|+tC|+cC|+gC])
  && all(\(n,call)->assertStateP (six' find' n ldR    ldT ldC ldG ldP  ldPr ldTr) call []) (zip' [r0,r1,r2,r3,r4,r5]   [rC,rC|+tC,rC|+tC|+cC,rC|+tC|+cC|+gC,rC|+tC|+cC|+gC|+pC|+pr3C,rC|+tC|+cC|+gC|+pC|+pr3C])

  && all(\(n,call)->assertStateP (seven' find' n ldRoot ldR ldT ldC ldG ldP ldPr ldTr) call []) (zip' [rt0,rt1,rt2,rt3,rt4,rt5,rt6] [rootC,rootC|+rC,rootC|+rC|+tC,rootC|+rC|+tC|+cC,rootC|+rC|+tC|+cC|+gC,rootC|+rC|+tC|+cC|+gC|+pC|+pr3C,rootC|+rC|+tC|+cC|+gC|+pC|+pr3C])

  && assertStateP (one' findUnambiguous' rt0   ldRoot ldR)  rootC Nothing
  && assertStateP (one' findUnambiguous' r0    ldR    ldT)  rC    Nothing
  && assertStateP (one' findUnambiguous' t0    ldT    ldC)  tC    Nothing
  && assertStateP (one' findUnambiguous' c0    ldC    ldG)  cC    Nothing
  && assertStateP (one' findUnambiguous' g0    ldG    ldP)  gC    Nothing
  && assertStateP (one' findUnambiguous' pTrr  ldPr   ldTr) prC   Nothing

  && all(\(n,call)->assertStateP (two' findUnambiguous' n ldRoot ldR  ldT)  call Nothing) (zip' [rt0,rt1] [rootC,rootC|+rC])
  && all(\(n,call)->assertStateP (two' findUnambiguous' n ldR    ldT  ldC)  call Nothing) (zip' [r0,r1]   [rC,rC|+tC])
  && all(\(n,call)->assertStateP (two' findUnambiguous' n ldT    ldC  ldG)  call Nothing) (zip' [t0,t1]   [tC,tC|+cC])
  && all(\(n,call)->assertStateP (two' findUnambiguous' n ldC    ldG  ldP)  call Nothing) (zip' [c0,c1]   [cC,cC|+gC])
  && all(\(n,call)->assertStateP (two' findUnambiguous' n ldG    ldP  ldPr) call Nothing) (zip' [g0]      [gC])
  && all(\(n,call)->assertStateP (two' findUnambiguous' n ldP    ldPr ldTr) call Nothing) (zip' [p0,p1]   [pC|+pr3C,pC|+pr3C])

  && all(\(n,call)->assertStateP (three' findUnambiguous' n ldRoot ldR ldT  ldC)  call Nothing) (zip' [rt0,rt1,rt2] [rootC,rootC|+rC,rootC|+rC|+tC])
  && all(\(n,call)->assertStateP (three' findUnambiguous' n ldR    ldT ldC  ldG)  call Nothing) (zip' [r0,r1,r2]    [rC,rC|+tC,rC|+tC|+cC])
  && all(\(n,call)->assertStateP (three' findUnambiguous' n ldT    ldC ldG  ldP)  call Nothing) (zip' [t0,t1,t2]    [tC,tC|+cC,tC|+cC|+gC])
  && all(\(n,call)->assertStateP (three' findUnambiguous' n ldG    ldP ldPr ldTr) call Nothing) (zip' [g0,g1,g2]    [gC,gC|+pC|+pr3C,gC|+pC|+pr3C])

  && all(\(n,call)->assertStateP (four' findUnambiguous' n ldRoot ldR ldT ldC  ldG)  call Nothing) (zip' [rt0,rt1,rt2,rt3] [rootC,rootC|+rC,rootC|+rC|+tC,rootC|+rC|+tC|+cC])
  && all(\(n,call)->assertStateP (four' findUnambiguous' n ldR    ldT ldC ldG  ldP)  call Nothing) (zip' [r0,r1,r2,r3]     [rC,rC|+tC,rC|+tC|+cC,rC|+tC|+cC|+gC])
  && all(\(n,call)->assertStateP (four' findUnambiguous' n ldT    ldC ldG ldP  ldPr) call Nothing) (zip' [t0,t1,t2]        [tC,tC|+cC,tC|+cC|+gC])
  && all(\(n,call)->assertStateP (four' findUnambiguous' n ldC    ldG ldP ldPr ldTr) call Nothing) (zip' [c0,c1,c2,c3]     [cC,cC|+gC,cC|+gC|+pC|+pr3C,cC|+gC|+pC|+pr3C])

  && all(\(n,call)->assertStateP (five' findUnambiguous' n ldRoot ldR ldT ldC  ldG  ldP)  call Nothing) (zip' [rt0,rt1,rt2,rt3,rt4] [rootC,rootC|+rC,rootC|+rC|+tC,rootC|+rC|+tC|+cC,rootC|+rC|+tC|+cC|+gC])
  && all(\(n,call)->assertStateP (five' findUnambiguous' n ldR    ldT ldC ldG  ldP  ldPr) call Nothing) (zip' [r0,r1,r2,r3]         [rC,rC|+tC,rC|+tC|+cC,rC|+tC|+cC|+gC])
  && all(\(n,call)->assertStateP (five' findUnambiguous' n ldT    ldC ldG ldP  ldPr ldTr) call Nothing) (zip' [t0,t1,t2,t3,t4]      [tC,tC|+cC,tC|+cC|+gC,tC|+cC|+gC|+pC|+pr3C,tC|+cC|+gC|+pC|+pr3C])

  && all(\(n,call)->assertStateP (six' findUnambiguous' n ldRoot ldR ldT ldC ldG  ldP  ldPr) call Nothing) (zip' [rt0,rt1,rt2,rt3,rt4] [rootC,rootC|+rC,rootC|+rC|+tC,rootC|+rC|+tC|+cC,rootC|+rC|+tC|+cC|+gC])
  && all(\(n,call)->assertStateP (six' findUnambiguous' n ldR    ldT ldC ldG ldP  ldPr ldTr) call Nothing) (zip' [r0,r1,r2,r3,r4,r5]   [rC,rC|+tC,rC|+tC|+cC,rC|+tC|+cC|+gC,rC|+tC|+cC|+gC|+pC|+pr3C,rC|+tC|+cC|+gC|+pC|+pr3C])

  && all(\(n,call)->assertStateP (seven' findUnambiguous' n ldRoot ldR ldT ldC ldG ldP ldPr ldTr) call Nothing) (zip' [rt0,rt1,rt2,rt3,rt4,rt5,rt6] [rootC,rootC|+rC,rootC|+rC|+tC,rootC|+rC|+tC|+cC,rootC|+rC|+tC|+cC|+gC,rootC|+rC|+tC|+cC|+gC|+pC|+pr3C,rootC|+rC|+tC|+cC|+gC|+pC|+pr3C])

prop_noFindAllGoodHints rootN rN tN cN gN pN trN = 
 "" `notElem` [rootN,rN,tN,cN,gN,pN,trN] ==>
  let rt0 = make rootN :: Root
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

  in assertStateP (one rN   find' rt0   ldRoot ldR)  rootC []
  && assertStateP (one tN   find' r0    ldR    ldT)  rC    []
  && assertStateP (one cN   find' t0    ldT    ldC)  tC    []
  && assertStateP (one gN   find' c0    ldC    ldG)  cC    []
  && assertStateP (one pN   find' g0    ldG    ldP)  gC    []
  && assertStateP (one trRN find' pTrr  ldPr   ldTr) prC   []

  && all(\(n,call)->assertStateP (two rN   tN   find' n ldRoot ldR  ldT)  call []) (zip' [rt0,rt1] [rootC,rootC|+rC])
  && all(\(n,call)->assertStateP (two tN   cN   find' n ldR    ldT  ldC)  call []) (zip' [r0,r1]   [rC,rC|+tC])
  && all(\(n,call)->assertStateP (two cN   gN   find' n ldT    ldC  ldG)  call []) (zip' [t0,t1]   [tC,tC|+cC])
  && all(\(n,call)->assertStateP (two gN   pN   find' n ldC    ldG  ldP)  call []) (zip' [c0,c1]   [cC,cC|+gC])
  && all(\(n,call)->assertStateP (two pN   trRN find' n ldG    ldP  ldPr) call []) (zip' [g0]      [gC])
  && all(\(n,call)->assertStateP (two trRN trN find' n ldP    ldPr ldTr)  call []) (zip' [p0,p1]   [pC|+prC,pC|+prC])

  && all(\(n,call)->assertStateP (three rN  tN  cN  find' n ldRoot ldR ldT  ldC)  call []) (zip' [rt0,rt1,rt2] [rootC,rootC|+rC,rootC|+rC|+tC])
  && all(\(n,call)->assertStateP (three tN  cN  gN  find' n ldR    ldT ldC  ldG)  call []) (zip' [r0,r1,r2]    [rC,rC|+tC,rC|+tC|+cC])
  && all(\(n,call)->assertStateP (three cN  gN  pN  find' n ldT    ldC ldG  ldP)  call []) (zip' [t0,t1,t2]    [tC,tC|+cC,tC|+cC|+gC])
  && all(\(n,call)->assertStateP (three pN trRN trN find' n ldG    ldP ldPr ldTr) call []) (zip' [g0,g1,g2]    [gC,gC|+pC|+prC,gC|+pC|+prC])

  && all(\(n,call)->assertStateP (four rN tN cN   gN   find' n ldRoot ldR ldT ldC  ldG)  call []) (zip' [rt0,rt1,rt2,rt3] [rootC,rootC|+rC,rootC|+rC|+tC,rootC|+rC|+tC|+cC])
  && all(\(n,call)->assertStateP (four tN cN gN   pN   find' n ldR    ldT ldC ldG  ldP)  call []) (zip' [r0,r1,r2,r3]     [rC,rC|+tC,rC|+tC|+cC,rC|+tC|+cC|+gC])
  && all(\(n,call)->assertStateP (four cN gN pN   trRN find' n ldT    ldC ldG ldP  ldPr) call []) (zip' [t0,t1,t2]        [tC,tC|+cC,tC|+cC|+gC])
  && all(\(n,call)->assertStateP (four gN pN trRN trN  find' n ldC    ldG ldP ldPr ldTr) call []) (zip' [c0,c1,c2,c3]     [cC,cC|+gC,cC|+gC|+pC|+prC,cC|+gC|+pC|+prC])

  && all(\(n,call)->assertStateP (five rN tN cN gN   pN   find' n ldRoot ldR ldT ldC  ldG  ldP)  call []) (zip' [rt0,rt1,rt2,rt3,rt4] [rootC,rootC|+rC,rootC|+rC|+tC,rootC|+rC|+tC|+cC,rootC|+rC|+tC|+cC|+gC])
  && all(\(n,call)->assertStateP (five tN cN gN pN   trRN find' n ldR    ldT ldC ldG  ldP  ldPr) call []) (zip' [r0,r1,r2,r3]         [rC,rC|+tC,rC|+tC|+cC,rC|+tC|+cC|+gC])
  && all(\(n,call)->assertStateP (five cN gN pN trRN trN  find' n ldT    ldC ldG ldP  ldPr ldTr) call []) (zip' [t0,t1,t2,t3,t4]      [tC,tC|+cC,tC|+cC|+gC,tC|+cC|+gC|+pC|+prC,tC|+cC|+gC|+pC|+prC])

  && all(\(n,call)->assertStateP (six rN tN cN gN pN   trRN find' n ldRoot ldR ldT ldC ldG  ldP  ldPr) call []) (zip' [rt0,rt1,rt2,rt3,rt4] [rootC,rootC|+rC,rootC|+rC|+tC,rootC|+rC|+tC|+cC,rootC|+rC|+tC|+cC|+gC])
  && all(\(n,call)->assertStateP (six tN cN gN pN trRN trN  find' n ldR    ldT ldC ldG ldP  ldPr ldTr) call []) (zip' [r0,r1,r2,r3,r4,r5]   [rC,rC|+tC,rC|+tC|+cC,rC|+tC|+cC|+gC,rC|+tC|+cC|+gC|+pC|+prC,rC|+tC|+cC|+gC|+pC|+prC])

  && all(\(n,call)->assertStateP (seven rN tN cN gN pN trRN trN find' n ldRoot ldR ldT ldC ldG ldP ldPr ldTr) call []) (zip' [rt0,rt1,rt2,rt3,rt4,rt5,rt6] [rootC,rootC|+rC,rootC|+rC|+tC,rootC|+rC|+tC|+cC,rootC|+rC|+tC|+cC|+gC,rootC|+rC|+tC|+cC|+gC|+pC|+prC,rootC|+rC|+tC|+cC|+gC|+pC|+prC])

  && assertStateP (one rN   findUnambiguous' rt0   ldRoot ldR)  rootC Nothing
  && assertStateP (one tN   findUnambiguous' r0    ldR    ldT)  rC    Nothing
  && assertStateP (one cN   findUnambiguous' t0    ldT    ldC)  tC    Nothing
  && assertStateP (one gN   findUnambiguous' c0    ldC    ldG)  cC    Nothing
  && assertStateP (one pN   findUnambiguous' g0    ldG    ldP)  gC    Nothing
  && assertStateP (one trRN findUnambiguous' pTrr  ldPr   ldTr) prC   Nothing

  && all(\(n,call)->assertStateP (two rN   tN   findUnambiguous' n ldRoot ldR  ldT)  call Nothing) (zip' [rt0,rt1] [rootC,rootC|+rC])
  && all(\(n,call)->assertStateP (two tN   cN   findUnambiguous' n ldR    ldT  ldC)  call Nothing) (zip' [r0,r1]   [rC,rC|+tC])
  && all(\(n,call)->assertStateP (two cN   gN   findUnambiguous' n ldT    ldC  ldG)  call Nothing) (zip' [t0,t1]   [tC,tC|+cC])
  && all(\(n,call)->assertStateP (two gN   pN   findUnambiguous' n ldC    ldG  ldP)  call Nothing) (zip' [c0,c1]   [cC,cC|+gC])
  && all(\(n,call)->assertStateP (two pN   trRN findUnambiguous' n ldG    ldP  ldPr) call Nothing) (zip' [g0]      [gC])
  && all(\(n,call)->assertStateP (two trRN trN findUnambiguous' n ldP    ldPr ldTr)  call Nothing) (zip' [p0,p1]   [pC|+prC,pC|+prC])

  && all(\(n,call)->assertStateP (three rN  tN  cN  findUnambiguous' n ldRoot ldR ldT  ldC)  call Nothing) (zip' [rt0,rt1,rt2] [rootC,rootC|+rC,rootC|+rC|+tC])
  && all(\(n,call)->assertStateP (three tN  cN  gN  findUnambiguous' n ldR    ldT ldC  ldG)  call Nothing) (zip' [r0,r1,r2]    [rC,rC|+tC,rC|+tC|+cC])
  && all(\(n,call)->assertStateP (three cN  gN  pN  findUnambiguous' n ldT    ldC ldG  ldP)  call Nothing) (zip' [t0,t1,t2]    [tC,tC|+cC,tC|+cC|+gC])
  && all(\(n,call)->assertStateP (three pN trRN trN findUnambiguous' n ldG    ldP ldPr ldTr) call Nothing) (zip' [g0,g1,g2]    [gC,gC|+pC|+prC,gC|+pC|+prC])

  && all(\(n,call)->assertStateP (four rN tN cN   gN   findUnambiguous' n ldRoot ldR ldT ldC  ldG)  call Nothing) (zip' [rt0,rt1,rt2,rt3] [rootC,rootC|+rC,rootC|+rC|+tC,rootC|+rC|+tC|+cC])
  && all(\(n,call)->assertStateP (four tN cN gN   pN   findUnambiguous' n ldR    ldT ldC ldG  ldP)  call Nothing) (zip' [r0,r1,r2,r3]     [rC,rC|+tC,rC|+tC|+cC,rC|+tC|+cC|+gC])
  && all(\(n,call)->assertStateP (four cN gN pN   trRN findUnambiguous' n ldT    ldC ldG ldP  ldPr) call Nothing) (zip' [t0,t1,t2]        [tC,tC|+cC,tC|+cC|+gC])
  && all(\(n,call)->assertStateP (four gN pN trRN trN  findUnambiguous' n ldC    ldG ldP ldPr ldTr) call Nothing) (zip' [c0,c1,c2,c3]     [cC,cC|+gC,cC|+gC|+pC|+prC,cC|+gC|+pC|+prC])

  && all(\(n,call)->assertStateP (five rN tN cN gN   pN   findUnambiguous' n ldRoot ldR ldT ldC  ldG  ldP)  call Nothing) (zip' [rt0,rt1,rt2,rt3,rt4] [rootC,rootC|+rC,rootC|+rC|+tC,rootC|+rC|+tC|+cC,rootC|+rC|+tC|+cC|+gC])
  && all(\(n,call)->assertStateP (five tN cN gN pN   trRN findUnambiguous' n ldR    ldT ldC ldG  ldP  ldPr) call Nothing) (zip' [r0,r1,r2,r3]         [rC,rC|+tC,rC|+tC|+cC,rC|+tC|+cC|+gC])
  && all(\(n,call)->assertStateP (five cN gN pN trRN trN  findUnambiguous' n ldT    ldC ldG ldP  ldPr ldTr) call Nothing) (zip' [t0,t1,t2,t3,t4]      [tC,tC|+cC,tC|+cC|+gC,tC|+cC|+gC|+pC|+prC,tC|+cC|+gC|+pC|+prC])

  && all(\(n,call)->assertStateP (six rN tN cN gN pN   trRN findUnambiguous' n ldRoot ldR ldT ldC ldG  ldP  ldPr) call Nothing) (zip' [rt0,rt1,rt2,rt3,rt4] [rootC,rootC|+rC,rootC|+rC|+tC,rootC|+rC|+tC|+cC,rootC|+rC|+tC|+cC|+gC])
  && all(\(n,call)->assertStateP (six tN cN gN pN trRN trN  findUnambiguous' n ldR    ldT ldC ldG ldP  ldPr ldTr) call Nothing) (zip' [r0,r1,r2,r3,r4,r5]   [rC,rC|+tC,rC|+tC|+cC,rC|+tC|+cC|+gC,rC|+tC|+cC|+gC|+pC|+prC,rC|+tC|+cC|+gC|+pC|+prC])

  && all(\(n,call)->assertStateP (seven rN tN cN gN pN trRN trN findUnambiguous' n ldRoot ldR ldT ldC ldG ldP ldPr ldTr) call Nothing) (zip' [rt0,rt1,rt2,rt3,rt4,rt5,rt6] [rootC,rootC|+rC,rootC|+rC|+tC,rootC|+rC|+tC|+cC,rootC|+rC|+tC|+cC|+gC,rootC|+rC|+tC|+cC|+gC|+pC|+prC,rootC|+rC|+tC|+cC|+gC|+pC|+prC])

prop_noFindAllBadHints rootN rN tN cN gN pN trN suffix = 
 "" `notElem` [rootN,rN,tN,cN,gN,pN,trN,suffix] ==>
  let tr = make (trN ++ suffix)
      p = setTrainRunRepo (make (pN ++ suffix)) trr
      g = addChild (make (gN ++ suffix)) p
      c = addChild (make (cN ++ suffix)) g
      t = addChild (make (tN ++ suffix)) c
      r = addChild (make (rN ++ suffix)) t
      root = addChild (make (rootN ++ suffix) :: Root) r
      
      trr = addChild emptyTrainRunRepo tr
      pTrr = makeProjectTrainRunRepo trr

      sRN = getName emptySubmitRepo ++ suffix
      tfRN = getName emptyTrainFileRepo ++ suffix
      trRN = getName emptyTrainRunRepo ++ suffix
      
  in assertStateP (one rN   find' root ldRoot ldR)  rootC []
  && assertStateP (one tN   find' r    ldR    ldT)  rC    []
  && assertStateP (one cN   find' t    ldT    ldC)  tC    []
  && assertStateP (one gN   find' c    ldC    ldG)  cC    []
  && assertStateP (one pN   find' g    ldG    ldP)  gC    []
  && assertStateP (one sRN  find' p    ldP    ldPr) pC    []
  && assertStateP (one tfRN find' p    ldP    ldPr) pC    []
  && assertStateP (one trRN find' p    ldP    ldPr) pC    []
  && assertStateP (one trN  find' pTrr ldPr   ldTr) prC   []

  && assertStateP (one rN   findUnambiguous' root ldRoot ldR)  rootC Nothing
  && assertStateP (one tN   findUnambiguous' r    ldR    ldT)  rC    Nothing
  && assertStateP (one cN   findUnambiguous' t    ldT    ldC)  tC    Nothing
  && assertStateP (one gN   findUnambiguous' c    ldC    ldG)  cC    Nothing
  && assertStateP (one pN   findUnambiguous' g    ldG    ldP)  gC    Nothing
  && assertStateP (one sRN  findUnambiguous' p    ldP    ldPr) pC    Nothing
  && assertStateP (one tfRN findUnambiguous' p    ldP    ldPr) pC    Nothing
  && assertStateP (one trRN findUnambiguous' p    ldP    ldPr) pC    Nothing
  && assertStateP (one trN  findUnambiguous' pTrr ldPr   ldTr) prC   Nothing

  && assertStateP (two rN   tN   find' root ldRoot ldR  ldT)  rootC []
  && assertStateP (two tN   cN   find' r    ldR    ldT  ldC)  rC    []
  && assertStateP (two cN   gN   find' t    ldT    ldC  ldG)  tC    []
  && assertStateP (two gN   pN   find' c    ldC    ldG  ldP)  cC    []
  && assertStateP (two pN   sRN  find' g    ldG    ldP  ldPr) gC    []
  && assertStateP (two pN   tfRN find' g    ldG    ldP  ldPr) gC    []
  && assertStateP (two pN   trRN find' g    ldG    ldP  ldPr) gC    []
  && assertStateP (two trRN trN  find' p    ldP    ldPr ldTr) pC    []

  && assertStateP (two rN   tN   findUnambiguous' root ldRoot ldR  ldT)  rootC Nothing
  && assertStateP (two tN   cN   findUnambiguous' r    ldR    ldT  ldC)  rC    Nothing
  && assertStateP (two cN   gN   findUnambiguous' t    ldT    ldC  ldG)  tC    Nothing
  && assertStateP (two gN   pN   findUnambiguous' c    ldC    ldG  ldP)  cC    Nothing
  && assertStateP (two pN   sRN  findUnambiguous' g    ldG    ldP  ldPr) gC    Nothing
  && assertStateP (two pN   tfRN findUnambiguous' g    ldG    ldP  ldPr) gC    Nothing
  && assertStateP (two pN   trRN findUnambiguous' g    ldG    ldP  ldPr) gC    Nothing
  && assertStateP (two trRN trN  findUnambiguous' p    ldP    ldPr ldTr) pC    Nothing

  && assertStateP (three rN tN   cN   find' root ldRoot ldR ldT  ldC)  rootC []
  && assertStateP (three tN cN   gN   find' r    ldR    ldT ldC  ldG)  rC    []
  && assertStateP (three cN gN   pN   find' t    ldT    ldC ldG  ldP)  tC    []
  && assertStateP (three gN pN   sRN  find' c    ldC    ldG ldP  ldPr) cC    []
  && assertStateP (three gN pN   tfRN find' c    ldC    ldG ldP  ldPr) cC    []
  && assertStateP (three gN pN   trRN find' c    ldC    ldG ldP  ldPr) cC    []
  && assertStateP (three pN trRN trN  find' g    ldG    ldP ldPr ldTr) gC    []

  && assertStateP (three rN tN   cN   findUnambiguous' root ldRoot ldR ldT  ldC)  rootC Nothing
  && assertStateP (three tN cN   gN   findUnambiguous' r    ldR    ldT ldC  ldG)  rC    Nothing
  && assertStateP (three cN gN   pN   findUnambiguous' t    ldT    ldC ldG  ldP)  tC    Nothing
  && assertStateP (three gN pN   sRN  findUnambiguous' c    ldC    ldG ldP  ldPr) cC    Nothing
  && assertStateP (three gN pN   tfRN findUnambiguous' c    ldC    ldG ldP  ldPr) cC    Nothing
  && assertStateP (three gN pN   trRN findUnambiguous' c    ldC    ldG ldP  ldPr) cC    Nothing
  && assertStateP (three pN trRN trN  findUnambiguous' g    ldG    ldP ldPr ldTr) gC    Nothing

  && assertStateP (four rN tN cN   gN   find' root ldRoot ldR ldT ldC  ldG)  rootC []
  && assertStateP (four tN cN gN   pN   find' r    ldR    ldT ldC ldG  ldP)  rC    []
  && assertStateP (four cN gN pN   sRN  find' t    ldT    ldC ldG ldP  ldPr) tC    []
  && assertStateP (four cN gN pN   tfRN find' t    ldT    ldC ldG ldP  ldPr) tC    []
  && assertStateP (four cN gN pN   trRN find' t    ldT    ldC ldG ldP  ldPr) tC    []
  && assertStateP (four gN pN trRN trN  find' c    ldC    ldG ldP ldPr ldTr) cC    []

  && assertStateP (four rN tN cN   gN   findUnambiguous' root ldRoot ldR ldT ldC  ldG)  rootC Nothing
  && assertStateP (four tN cN gN   pN   findUnambiguous' r    ldR    ldT ldC ldG  ldP)  rC    Nothing
  && assertStateP (four cN gN pN   sRN  findUnambiguous' t    ldT    ldC ldG ldP  ldPr) tC    Nothing
  && assertStateP (four cN gN pN   tfRN findUnambiguous' t    ldT    ldC ldG ldP  ldPr) tC    Nothing
  && assertStateP (four cN gN pN   trRN findUnambiguous' t    ldT    ldC ldG ldP  ldPr) tC    Nothing
  && assertStateP (four gN pN trRN trN  findUnambiguous' c    ldC    ldG ldP ldPr ldTr) cC    Nothing

  && assertStateP (five rN tN cN gN   pN   find' root ldRoot ldR ldT ldC ldG  ldP)  rootC []
  && assertStateP (five tN cN gN pN   sRN  find' r    ldR    ldT ldC ldG ldP  ldPr) rC    []
  && assertStateP (five tN cN gN pN   tfRN find' r    ldR    ldT ldC ldG ldP  ldPr) rC    []
  && assertStateP (five tN cN gN pN   trRN find' r    ldR    ldT ldC ldG ldP  ldPr) rC    []
  && assertStateP (five cN gN pN trRN trN  find' t    ldT    ldC ldG ldP ldPr ldTr) tC    []

  && assertStateP (five rN tN cN gN   pN   findUnambiguous' root ldRoot ldR ldT ldC ldG  ldP)  rootC  Nothing
  && assertStateP (five tN cN gN pN   sRN  findUnambiguous' r    ldR    ldT ldC ldG ldP  ldPr) rC     Nothing
  && assertStateP (five tN cN gN pN   tfRN findUnambiguous' r    ldR    ldT ldC ldG ldP  ldPr) rC     Nothing
  && assertStateP (five tN cN gN pN   trRN findUnambiguous' r    ldR    ldT ldC ldG ldP  ldPr) rC     Nothing
  && assertStateP (five cN gN pN trRN trN  findUnambiguous' t    ldT    ldC ldG ldP ldPr ldTr) tC     Nothing

  && assertStateP (six rN tN cN gN pN   sRN  find' root ldRoot ldR ldT ldC ldG ldP  ldPr) rootC []
  && assertStateP (six rN tN cN gN pN   tfRN find' root ldRoot ldR ldT ldC ldG ldP  ldPr) rootC []
  && assertStateP (six rN tN cN gN pN   trRN find' root ldRoot ldR ldT ldC ldG ldP  ldPr) rootC []
  && assertStateP (six tN cN gN pN trRN trN  find' r    ldR    ldT ldC ldG ldP ldPr ldTr) rC    []

  && assertStateP (six rN tN cN gN pN   sRN  findUnambiguous' root ldRoot ldR ldT ldC ldG ldP  ldPr) rootC Nothing
  && assertStateP (six rN tN cN gN pN   tfRN findUnambiguous' root ldRoot ldR ldT ldC ldG ldP  ldPr) rootC Nothing
  && assertStateP (six rN tN cN gN pN   trRN findUnambiguous' root ldRoot ldR ldT ldC ldG ldP  ldPr) rootC Nothing
  && assertStateP (six tN cN gN pN trRN trN  findUnambiguous' r    ldR    ldT ldC ldG ldP ldPr ldTr) rC    Nothing

  && assertStateP (seven rN tN cN gN pN trRN trN find'            root ldRoot ldR ldT ldC ldG ldP ldPr ldTr) rootC  []
  && assertStateP (seven rN tN cN gN pN trRN trN findUnambiguous' root ldRoot ldR ldT ldC ldG ldP ldPr ldTr) rootC  Nothing
