{-# OPTIONS_GHC -F -pgmF htfpp -fno-warn-incomplete-patterns#-}
module Infrastructure.Finder.FinderPropNoFindingTest where

import Test.Framework
import Data.List hiding (find)
import Control.Arrow (first)
import TestUtils
import Data.Maybe

import Infrastructure.Node
import Domain.Root
import Domain.Project
import Domain.ProjectRepo

import Infrastructure.Finder

{-# ANN module "HLint: ignore Use camelCase" #-}
{-# ANN module "HLint: ignore Reduce duplication" #-}

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
      [p1,p2] = map (setTrainRunRepo p0) [trr0,trr1]
      trr0 = emptyTrainRunRepo
      [trr1] = map (addChild trr0) [tr0]
      tr0 = make trN

      prs = [pSubmitR,pTrainFileR,pTrainRunR]
      prs1 = map (\x -> (K pN Z,x)) prs
      prs2 = map (\x -> (K pN $ K gN Z,x)) prs
      prs3 = map (\x -> (K pN $ K gN $ K cN Z,x)) prs
      prs4 = map (\x -> (K pN $ K gN $ K cN $ K tN Z,x)) prs
      prs5 = map (\x -> (K pN $ K gN $ K cN $ K tN $ K rN Z,x)) prs
      prs6 = map (\x -> (K pN $ K gN $ K cN $ K tN $ K rN $ K rootN Z,x)) prs
      pSubmitR = makeProjectSubmitRepo emptySubmitRepo
      pTrainFileR = makeProjectTrainFileRepo emptyTrainFileRepo
      pTrainRunR = makeProjectTrainRunRepo trr0 in

     [[]] == nub (map (find one') [rt0])                           && [Nothing] == nub (map (findUnambiguous one') [rt0])
  && [[]] == nub (map (find one') [r0])                            && [Nothing] == nub (map (findUnambiguous one') [r0])
  && [[]] == nub (map (find one') [t0])                            && [Nothing] == nub (map (findUnambiguous one') [t0])
  && [[]] == nub (map (find one') [c0])                            && [Nothing] == nub (map (findUnambiguous one') [c0])
  && [[]] == nub (map (find one') [g0])                            && [Nothing] == nub (map (findUnambiguous one') [g0])
  && [[]] == nub (map (find one') [trr0])                          && [Nothing] == nub (map (findUnambiguous one') [trr0])
  && prs1 == find one' p0

  && [[]] == nub (map (find two') [rt0,rt1])                       && [Nothing] == nub (map (findUnambiguous two') [rt0,rt1])
  && [[]] == nub (map (find two') [r0,r1])                         && [Nothing] == nub (map (findUnambiguous two') [r0,r1])
  && [[]] == nub (map (find two') [t0,t1])                         && [Nothing] == nub (map (findUnambiguous two') [t0,t1])
  && [[]] == nub (map (find two') [c0,c1])                         && [Nothing] == nub (map (findUnambiguous two') [c0,c1])
  && [[]] == nub (map (find two') [g0])                            && [Nothing] == nub (map (findUnambiguous two') [g0])
  && [[]] == nub (map (find two') [p0,p1])                         && [Nothing] == nub (map (findUnambiguous two') [p0,p1])
  && prs2 == find two' g1

  && [[]] == nub (map (find three') [rt0,rt1,rt2])                 && [Nothing] == nub (map (findUnambiguous three') [rt0,rt1,rt2])
  && [[]] == nub (map (find three') [r0,r1,r2])                    && [Nothing] == nub (map (findUnambiguous three') [r0,r1,r2])
  && [[]] == nub (map (find three') [t0,t1,t2])                    && [Nothing] == nub (map (findUnambiguous three') [t0,t1,t2])
  && [[]] == nub (map (find three') [c0,c1])                       && [Nothing] == nub (map (findUnambiguous three') [c0,c1])
  && [[]] == nub (map (find three') [g0,g1,g2])                    && [Nothing] == nub (map (findUnambiguous three') [g0,g1,g2])
  && prs3 == find three' c2

  && [[]] == nub (map (find four') [rt0,rt1,rt2,rt3])              && [Nothing] == nub (map (findUnambiguous four') [rt0,rt1,rt2,rt3])
  && [[]] == nub (map (find four') [r0,r1,r2,r3])                  && [Nothing] == nub (map (findUnambiguous four') [r0,r1,r2,r3])
  && [[]] == nub (map (find four') [t0,t1,t2])                     && [Nothing] == nub (map (findUnambiguous four') [t0,t1,t2])
  && [[]] == nub (map (find four') [c0,c1,c2,c3])                  && [Nothing] == nub (map (findUnambiguous four') [c0,c1,c2,c3])
  && prs4 == find four' t3

  && [[]] == nub (map (find five') [rt0,rt1,rt2,rt3,rt4])          && [Nothing] == nub (map (findUnambiguous five') [rt0,rt1,rt2,rt3,rt4])
  && [[]] == nub (map (find five') [r0,r1,r2,r3])                  && [Nothing] == nub (map (findUnambiguous five') [r0,r1,r2,r3])
  && [[]] == nub (map (find five') [t0,t1,t2,t3,t4])               && [Nothing] == nub (map (findUnambiguous five') [t0,t1,t2,t3,t4])
  && prs5 == find five' r4

  && [[]] == nub (map (find six') [rt0,rt1,rt2,rt3,rt4])           && [Nothing] == nub (map (findUnambiguous six') [rt0,rt1,rt2,rt3,rt4])
  && [[]] == nub (map (find six') [r0,r1,r2,r3,r4,r5])             && [Nothing] == nub (map (findUnambiguous six') [r0,r1,r2,r3,r4,r5])
  && prs6 == find six' rt5

  && [[]] == nub (map (find seven') [rt0,rt1,rt2,rt3,rt4,rt5,rt6]) && [Nothing] == nub (map (findUnambiguous seven') [rt0,rt1,rt2,rt3,rt4,rt5,rt6])

prop_noFindAllGoodHints rootN rN tN cN gN pN trN =
 "" `notElem` [rootN,rN,tN,cN,gN,pN,trN] ==>
  let trRN = getName emptyTrainRunRepo

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
      tr0 = make trN in

     [[]] == nub (map (find (one rN))  [rt0])                                                     && [Nothing] == nub (map (findUnambiguous (one rN))  [rt0])
  && [[]] == nub (map (find (one tN))  [r0])                                                      && [Nothing] == nub (map (findUnambiguous (one tN))  [r0])
  && [[]] == nub (map (find (one cN))  [t0])                                                      && [Nothing] == nub (map (findUnambiguous (one cN))  [t0])
  && [[]] == nub (map (find (one gN))  [c0])                                                      && [Nothing] == nub (map (findUnambiguous (one gN))  [c0])
  && [[]] == nub (map (find (one pN))  [g0])                                                      && [Nothing] == nub (map (findUnambiguous (one pN))  [g0])
  && [[]] == nub (map (find (one trN)) [trr0])                                                    && [Nothing] == nub (map (findUnambiguous (one trN)) [trr0])

  && [[]] == nub (map (find (two rN tN))    [rt0,rt1])                                            && [Nothing] == nub (map (findUnambiguous (two rN tN))    [rt0,rt1])
  && [[]] == nub (map (find (two tN cN))    [r0,r1])                                              && [Nothing] == nub (map (findUnambiguous (two tN cN))    [r0,r1])
  && [[]] == nub (map (find (two cN gN))    [t0,t1])                                              && [Nothing] == nub (map (findUnambiguous (two cN gN))    [t0,t1])
  && [[]] == nub (map (find (two gN pN))    [c0,c1])                                              && [Nothing] == nub (map (findUnambiguous (two gN pN))    [c0,c1])
  && [[]] == nub (map (find (two pN trRN))  [p0,p1])                                              && [Nothing] == nub (map (findUnambiguous (two pN trRN))  [p0,p1])

  && [[]] == nub (map (find (three rN tN cN))    [rt0,rt1,rt2])                                   && [Nothing] == nub (map (findUnambiguous (three rN tN cN))    [rt0,rt1,rt2])
  && [[]] == nub (map (find (three tN cN gN))    [r0,r1,r2])                                      && [Nothing] == nub (map (findUnambiguous (three tN cN gN))    [r0,r1,r2])
  && [[]] == nub (map (find (three cN gN pN))    [t0,t1,t2])                                      && [Nothing] == nub (map (findUnambiguous (three cN gN pN))    [t0,t1,t2])
  && [[]] == nub (map (find (three pN trRN trN)) [g0,g1,g2])                                      && [Nothing] == nub (map (findUnambiguous (three pN trRN trN)) [g0,g1,g2])

  && [[]] == nub (map (find (four rN tN cN gN))    [rt0,rt1,rt2,rt3])                             && [Nothing] == nub (map (findUnambiguous (four rN tN cN gN))    [rt0,rt1,rt2,rt3])
  && [[]] == nub (map (find (four tN cN gN pN))    [r0,r1,r2,r3])                                 && [Nothing] == nub (map (findUnambiguous (four tN cN gN pN))    [r0,r1,r2,r3])
  && [[]] == nub (map (find (four cN gN pN trN))   [t0,t1,t2,t3])                                 && [Nothing] == nub (map (findUnambiguous (four cN gN pN trN))   [t0,t1,t2,t3])
  && [[]] == nub (map (find (four gN pN trRN trN)) [c0,c1,c2,c3])                                 && [Nothing] == nub (map (findUnambiguous (four gN pN trRN trN)) [c0,c1,c2,c3])

  && [[]] == nub (map (find (five rN tN cN gN pN))    [rt0,rt1,rt2,rt3,rt4])                      && [Nothing] == nub (map (findUnambiguous (five rN tN cN gN pN))    [rt0,rt1,rt2,rt3,rt4])
  && [[]] == nub (map (find (five tN cN gN pN trN))   [r0,r1,r2,r3,r4])                           && [Nothing] == nub (map (findUnambiguous (five tN cN gN pN trN))   [r0,r1,r2,r3,r4])
  && [[]] == nub (map (find (five cN gN pN trRN trN)) [t0,t1,t2,t3,t4])                           && [Nothing] == nub (map (findUnambiguous (five cN gN pN trRN trN)) [t0,t1,t2,t3,t4])

  && [[]] == nub (map (find (six rN cN gN pN trRN trN))  [rt0,rt1,rt2,rt3,rt4,rt5])               && [Nothing] == nub (map (findUnambiguous (six rN cN gN pN trRN trN))  [rt0,rt1,rt2,rt3,rt4,rt5])
  && [[]] == nub (map (find (six cN gN pN trRN trN trN)) [r0,r1,r2,r3,r4,r5])                     && [Nothing] == nub (map (findUnambiguous (six cN gN pN trRN trN trN)) [r0,r1,r2,r3,r4,r5])

  && [[]] == nub (map (find (seven rN tN cN gN pN trRN trN)) [rt0, rt1, rt2, rt3, rt4, rt5, rt6]) && [Nothing] == nub (map (findUnambiguous (seven rN tN cN gN pN trRN trN)) [rt0, rt1, rt2, rt3, rt4, rt5, rt6])

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

      sRN = getName emptySubmitRepo ++ suffix
      tfRN = getName emptyTrainFileRepo ++ suffix
      trRN = getName emptyTrainRunRepo ++ suffix in

     [] == find (one rN) rt                        && isNothing(findUnambiguous (one rN) rt)
  && [] == find (one tN) r                         && isNothing(findUnambiguous (one tN) r)
  && [] == find (one cN) t                         && isNothing(findUnambiguous (one cN) t)
  && [] == find (one gN) c                         && isNothing(findUnambiguous (one gN) c)
  && [] == find (one pN) g                         && isNothing(findUnambiguous (one pN) g)
  && [] == find (one sRN) p                        && isNothing(findUnambiguous (one sRN) p)
  && [] == find (one tfRN) p                       && isNothing(findUnambiguous (one tfRN) p)
  && [] == find (one trRN) p                       && isNothing(findUnambiguous (one trRN) p)
  && [] == find (one trN) trr                      && isNothing(findUnambiguous (one trN) trr)

  && [] == find (two rN tN) rt                     && isNothing(findUnambiguous (two rN tN) rt)
  && [] == find (two tN cN) r                      && isNothing(findUnambiguous (two tN cN) r)
  && [] == find (two cN gN) t                      && isNothing(findUnambiguous (two cN gN) t)
  && [] == find (two gN pN) c                      && isNothing(findUnambiguous (two gN pN) c)
  && [] == find (two pN sRN) g                     && isNothing(findUnambiguous (two pN sRN) g)
  && [] == find (two pN tfRN) g                    && isNothing(findUnambiguous (two pN tfRN) g)
  && [] == find (two pN trRN) g                    && isNothing(findUnambiguous (two pN trRN) g)
  && [] == find (two pN trRN) p                    && isNothing(findUnambiguous (two pN trRN) p)

  && [] == find (three rN tN cN) rt                && isNothing(findUnambiguous (three rN tN cN) rt)
  && [] == find (three tN cN gN) r                 && isNothing(findUnambiguous (three tN cN gN) r)
  && [] == find (three cN gN pN) t                 && isNothing(findUnambiguous (three cN gN pN) t)
  && [] == find (three gN pN sRN) c                && isNothing(findUnambiguous (three gN pN sRN) c)
  && [] == find (three gN pN tfRN) c               && isNothing(findUnambiguous (three gN pN tfRN) c)
  && [] == find (three gN pN trRN) c               && isNothing(findUnambiguous (three gN pN trRN) c)
  && [] == find (three pN trRN trN) g              && isNothing(findUnambiguous (three pN trRN trN) g)

  && [] == find (four rN tN cN gN) rt              && isNothing(findUnambiguous (four rN tN cN gN) rt)
  && [] == find (four tN cN gN pN) r               && isNothing(findUnambiguous (four tN cN gN pN) r)
  && [] == find (four cN gN pN sRN) t              && isNothing(findUnambiguous (four cN gN pN sRN) t)
  && [] == find (four cN gN pN tfRN) t             && isNothing(findUnambiguous (four cN gN pN tfRN) t)
  && [] == find (four cN gN pN trRN) t             && isNothing(findUnambiguous (four cN gN pN trRN) t)
  && [] == find (four gN pN trRN trN) c            && isNothing(findUnambiguous (four gN pN trRN trN) c)

  && [] == find (five rN tN cN gN pN) rt           && isNothing(findUnambiguous (five rN tN cN gN pN) rt)
  && [] == find (five tN cN gN pN sRN) r           && isNothing(findUnambiguous (five tN cN gN pN sRN) r)
  && [] == find (five tN cN gN pN tfRN) r          && isNothing(findUnambiguous (five tN cN gN pN tfRN) r)
  && [] == find (five tN cN gN pN trRN) r          && isNothing(findUnambiguous (five tN cN gN pN trRN) r)
  && [] == find (five cN gN pN trRN trN) t         && isNothing(findUnambiguous (five cN gN pN trRN trN) t)

  && [] == find (six rN tN cN gN pN trRN) rt       && isNothing(findUnambiguous (six rN tN cN gN pN trRN) rt)
  && [] == find (six rN tN cN gN pN tfRN) rt       && isNothing(findUnambiguous (six rN tN cN gN pN tfRN) rt)
  && [] == find (six rN tN cN gN pN sRN) rt        && isNothing(findUnambiguous (six rN tN cN gN pN sRN) rt)
  && [] == find (six rN cN gN pN trRN trN) r       && isNothing(findUnambiguous (six rN cN gN pN trRN trN) r)

  && [] == find (seven rN tN cN gN pN trRN trN) rt && isNothing(findUnambiguous (seven rN tN cN gN pN trRN trN) rt)


prop_noFindGeneral suf rootName ns = let xs = filter (not.null) $ nub (suf:ns) in length xs > 1 ==> let (suffix:names) = xs in
  let root = makeNode rootName 4 7 names
  in  all (uncurry (absentOnFind suffix root)) $ makeKeys root

absentOnFind :: String -> Node -> Node -> [String] -> Bool
absentOnFind _   rt n [_]                 = noFindWhenNoChildren rt n []
absentOnFind suf rt n [_,r]               = noFindWhenNoChildren rt n [r]               && noFindByName suf rt (badOnes suf r)
absentOnFind suf rt n [_,r,t]             = noFindWhenNoChildren rt n [r,t]             && noFindByName suf rt (badTwos suf r t)
absentOnFind suf rt n [_,r,t,c]           = noFindWhenNoChildren rt n [r,t,c]           && noFindByName suf rt (badThrees suf r t c)
absentOnFind suf rt n [_,r,t,c,g]         = noFindWhenNoChildren rt n [r,t,c,g]         && noFindByName suf rt (badFours suf r t c g)
absentOnFind suf rt n [_,r,t,c,g,p]       = noFindWhenNoChildren rt n [r,t,c,g,p]       && noFindByName suf rt (badFives suf r t c g p)
absentOnFind suf rt n [_,r,t,c,g,p,pr]    = noFindWhenNoChildren rt n [r,t,c,g,p,pr]    && noFindByName suf rt (badSixes suf r t c g p pr)
absentOnFind suf rt n [_,r,t,c,g,p,pr,tr] = noFindWhenNoChildren rt n [r,t,c,g,p,pr,tr] && noFindByName suf rt (badSevens suf r t c g p pr tr)

noFindByName bad rt allPossibilities = all f (filter (contains bad) allPossibilities)
 where f s = [] == find s rt && isNothing (findUnambiguous s rt)

class Contains a where contains :: String -> a -> Bool
instance Contains Z where contains _ Z = False
instance Contains s => Contains (S s) where contains s (S ms x)  = maybe False (==s) ms || contains s x

noFindWhenNoChildren rt n args = getChildren n /= [] || findNoneFromRoot rt args
findNoneFromRoot rt [] = null(find one' rt)   && isNothing(findUnambiguous one' rt)
                      && null(find two' rt)   && isNothing(findUnambiguous two' rt)
                      && null(find three' rt) && isNothing(findUnambiguous three' rt)
                      && null(find four' rt)  && isNothing(findUnambiguous four' rt)
                      && null(find five' rt)  && isNothing(findUnambiguous five' rt)
                      && null(find six' rt)   && isNothing(findUnambiguous six' rt)
                      && null(find seven' rt) && isNothing(findUnambiguous seven' rt)

findNoneFromRoot rt [r] = null(find (S z$ one r) rt)                     && isNothing(findUnambiguous (S z$ one r) rt)
                       && null(find (S z$S z$ one r) rt)                 && isNothing(findUnambiguous (S z$S z$ one r) rt)
                       && null(find (S z$S z$S z$ one r) rt)             && isNothing(findUnambiguous (S z$S z$S z$ one r) rt)
                       && null(find (S z$S z$S z$S z $one r) rt)         && isNothing(findUnambiguous (S z$S z$S z$S z $one r) rt)
                       && null(find (S z$S z$S z$S z$S z$ one r) rt)     && isNothing(findUnambiguous (S z$S z$S z$S z$S z$ one r) rt)
                       && null(find (S z$S z$S z$S z$S z$S z$ one r) rt) && isNothing(findUnambiguous (S z$S z$S z$S z$S z$S z$ one r) rt)

findNoneFromRoot rt [r,t] = null(find (S z$ two r t) rt)                 && isNothing(findUnambiguous (S z$ two r t) rt)
                         && null(find (S z$S z$ two r t) rt)             && isNothing(findUnambiguous (S z$S z$ two r t) rt)
                         && null(find (S z$S z$S z $ two r t) rt)        && isNothing(findUnambiguous (S z$S z$S z $ two r t) rt)
                         && null(find (S z$S z$S z$S z$ two r t) rt)     && isNothing(findUnambiguous (S z$S z$S z$S z$ two r t) rt)
                         && null(find (S z$S z$S z$S z$S z$ two r t) rt) && isNothing(findUnambiguous (S z$S z$S z$S z$S z$ two r t) rt)

findNoneFromRoot rt [r,t,c] = null(find (S z$ three r t c) rt)             && isNothing(findUnambiguous (S z$ three r t c) rt)
                           && null(find (S z$S z $ three r t c) rt)        && isNothing(findUnambiguous (S z$S z $ three r t c) rt)
                           && null(find (S z$S z$S z$ three r t c) rt)     && isNothing(findUnambiguous (S z$S z$S z$ three r t c) rt)
                           && null(find (S z$S z$S z$S z$ three r t c) rt) && isNothing(findUnambiguous (S z$S z$S z$S z$ three r t c) rt)

findNoneFromRoot rt [r,t,c,g] = null(find (S z $ four r t c g) rt)        && isNothing(findUnambiguous (S z $ four r t c g) rt)
                             && null(find (S z$S z$ four r t c g) rt)     && isNothing(findUnambiguous (S z$S z$ four r t c g) rt)
                             && null(find (S z$S z$S z$ four r t c g) rt) && isNothing(findUnambiguous (S z$S z$S z$ four r t c g) rt)

findNoneFromRoot rt [r,t,c,g,p] = null(find (S z$ five r t c g p) rt)     && isNothing(findUnambiguous (S z$ five r t c g p) rt)
                               && null(find (S z$S z$ five r t c g p) rt) && isNothing(findUnambiguous (S z$S z$ five r t c g p) rt)

findNoneFromRoot rt [r,t,c,g,p,pr] = null(find (S z$ six r t c g p pr) rt) && isNothing(findUnambiguous (S z$ six r t c g p pr) rt)



badOnes w a               = [S x Z | x <- trip w a]
badTwos w a b             = [S x s | x <- trip w a, s <- badOnes w b]
badThrees w a b c         = [S x s | x <- trip w a, s <- badTwos w b c]
badFours w a b c d        = [S x s | x <- trip w a, s <- badThrees w b c d]
badFives w a b c d e      = [S x s | x <- trip w a, s <- badFours w b c d e]
badSixes w a b c d e f    = [S x s | x <- trip w a, s <- badFives w b c d e f]
badSevens w a b c d e f g = [S x s | x <- trip w a, s <- badSixes w b c d e f g]
trip w a = [Just w, Just a, Nothing]

z = Nothing
one' = S z zero
two' = S z one'
three' = S z two'
four' = S z three'
five' = S z four'
six' = S z five'
seven' = S z six'

zero = Z
one a = S (Just a) zero
two a b = S (Just a) $ one b
three a b c = S (Just a) $ two b c
four a b c d = S (Just a) $ three b c d
five a b c d e = S (Just a) $ four b c d e
six a b c d e f = S (Just a) $ five b c d e f
seven a b c d e f g = S (Just a) $ six b c d e f g

