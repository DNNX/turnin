{-# OPTIONS_GHC -F -pgmF htfpp -fno-warn-incomplete-patterns#-}
module Infrastructure.FinderPropFindingTest where

import Test.Framework
import TestUtils
import Data.List hiding (find)

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
      pSubmitR = makeProjectSubmitRepo emptySubmitRepo
      pTrainFileR = makeProjectTrainFileRepo emptyTrainFileRepo
      pTrainRunR = makeProjectTrainRunRepo trr
      prs = [pSubmitR,pTrainFileR,pTrainRunR]
      p = setTrainRunRepo (make pN) trr
      g = addChild (make gN) p
      c = addChild (make cN) g
      t = addChild (make tN) c
      r = addChild (make rN) t
      root = addChild (make rootN :: Root) r in
  [root] == find zero root &&
  [[r]]  == nub [find zero r, find one' root] &&
  [[t]]  == nub [find zero t, find one' r, find two' root] &&
  [[c]]  == nub [find zero c, find one' t, find two' r, find three' root] &&
  [[g]]  == nub [find zero g, find one' c, find two' t, find three' r, find four' root] &&
  [[p]]  == nub [find zero p, find one' g, find two' c, find three' t, find four' r, find five' root] &&
  [trr]  == find zero trr &&
  [prs]  == nub [find one' p, find two' g, find three' c, find four' t, find five' r, find six' root] &&
  [[tr]] == nub [find zero tr, find one' trr, find two' p, find three' g, find four' c, find five' t, find six' r, find seven' root]

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
  [[r]]           == nub [find (one rN) root] &&
  [[t]]           == nub [find (one tN) r,    find (two rN tN) root] &&
  [[c]]           == nub [find (one cN) t,    find (two tN cN) r,    find (three rN tN cN) root] &&
  [[g]]           == nub [find (one gN) c,    find (two cN gN) t,    find (three tN cN gN) r,    find (four rN tN cN gN) root] &&
  [[p]]           == nub [find (one pN) g,    find (two gN pN) c,    find (three cN gN pN) t,    find (four tN cN gN pN) r,    find (five rN tN cN gN pN) root] &&
  [[pSubmitR]]    == nub [find (one sRN) p,   find (two pN sRN) g,   find (three gN pN sRN) c,   find (four cN gN pN sRN) t,   find (five tN cN gN pN sRN) r,   find (six rN tN cN gN pN sRN) root] &&
  [[pTrainFileR]] == nub [find (one tfRN) p,  find (two pN tfRN) g,  find (three gN pN tfRN) c,  find (four cN gN pN tfRN) t,  find (five tN cN gN pN tfRN) r,  find (six rN tN cN gN pN tfRN) root] &&
  [[pTrainRunR]]  == nub [find (one trRN) p,  find (two pN trRN) g,  find (three gN pN trRN) c,  find (four cN gN pN trRN) t,  find (five tN cN gN pN trRN) r,  find (six rN tN cN gN pN trRN) root] &&
  [[tr]]          == nub [find (one trN) trr, find (two trRN trN) p, find (three pN trRN trN) g, find (four gN pN trRN trN) c, find (five cN gN pN trRN trN) t, find (six tN cN gN pN trRN trN) r,  find (seven rN tN cN gN pN trRN trN)root]

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


