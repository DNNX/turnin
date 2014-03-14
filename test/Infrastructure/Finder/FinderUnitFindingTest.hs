{-# OPTIONS_GHC -F -pgmF htfpp -fno-warn-incomplete-patterns#-}
module Infrastructure.Finder.FinderUnitFindingTest where

import Test.Framework
import Control.Arrow (first)

import Infrastructure.Node
import Domain.Root
import Domain.Project
import Domain.ProjectRepo

import Infrastructure.Finder

{-# ANN module "HLint: ignore Use camelCase" #-}
{-# ANN module "HLint: ignore Reduce duplication" #-}
{-# ANN module "HLint: ignore Evaluate" #-}
{-# ANN module "HLint: ignore Use ++" #-}

test_findNoHints = do
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

      rootN = "root"
      rN = "repo"
      tN = "term"
      cN = "course"
      gN = "group"
      pN = "project"
      trN = "trainRun"

      trrN = getName trr

  assertEqual [(Z,root)] $ find zero root
  assertEqual [(Z,r)]    $ find zero r
  assertEqual [(Z,t)]    $ find zero t
  assertEqual [(Z,c)]    $ find zero c
  assertEqual [(Z,g)]    $ find zero g
  assertEqual [(Z,p)]    $ find zero p
  assertEqual [(Z,trr)]  $ find zero trr
  assertEqual [(Z,tr)]   $ find zero tr

  assertEqual [(one rootN,r)] $ find one' root
  assertEqual [(one rN,t)]    $ find one' r
  assertEqual [(one tN,c)]    $ find one' t
  assertEqual [(one cN,g)]    $ find one' c
  assertEqual [(one gN,p)]    $ find one' g
  assertEqual prs1            $ find one' p
  assertEqual [(one trrN,tr)] $ find one' trr

  assertEqual [(two rootN rN,t)] $ find two' root
  assertEqual [(two rN tN,c)]    $ find two' r
  assertEqual [(two tN cN,g)]    $ find two' t
  assertEqual [(two cN gN,p)]    $ find two' c
  assertEqual prs2               $ find two' g
  assertEqual [(two pN trrN,tr)] $ find two' p

  assertEqual [(three rootN rN tN,c)] $ find three' root
  assertEqual [(three rN tN cN,g)]    $ find three' r
  assertEqual [(three tN cN gN,p)]    $ find three' t
  assertEqual prs3                    $ find three' c
  assertEqual [(three gN pN trrN,tr)] $ find three' g

  assertEqual [(four rootN rN tN cN,g)]  $ find four' root
  assertEqual [(four rN tN cN gN,p)]     $ find four' r
  assertEqual prs4                       $ find four' t
  assertEqual [(four cN gN pN trrN, tr)] $ find four' c

  assertEqual [(five rootN rN tN cN gN,p)] $ find five' root
  assertEqual prs5                         $ find five' r
  assertEqual [(five tN cN gN pN trrN,tr)] $ find five' t

  assertEqual prs6                        $ find six' root
  assertEqual [(six rN tN cN gN pN trrN,tr)] $ find six' r

  assertEqual [(seven rootN rN tN cN gN pN trrN,tr)] $ find seven' root

test_findAllHints = do
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

      rootN = "root"
      rN = "repo"
      tN = "term"
      cN = "course"
      gN = "group"
      pN = "project"
      trN = "trainRun"
      sRN = getName emptySubmitRepo
      tfRN = getName emptyTrainFileRepo
      trRN = getName emptyTrainRunRepo
  assertEqual [(one rootN,r)]        $ find (one rN) root
  assertEqual [(one rN,t)]           $ find (one tN) r
  assertEqual [(one tN,c)]           $ find (one cN) t
  assertEqual [(one cN,g)]           $ find (one gN) c
  assertEqual [(one gN,p)]           $ find (one pN) g
  assertEqual [(one pN,pSubmitR)]    $ find (one sRN) p
  assertEqual [(one pN,pTrainFileR)] $ find (one tfRN) p
  assertEqual [(one pN,pTrainRunR)]  $ find (one trRN) p
  assertEqual [(one trRN,tr)]        $ find (one trN) trr

  assertEqual [(two rootN rN,t)]        $ find (two rN tN) root
  assertEqual [(two rN tN,c)]           $ find (two tN cN) r
  assertEqual [(two tN cN,g)]           $ find (two cN gN) t
  assertEqual [(two cN gN,p)]           $ find (two gN pN) c
  assertEqual [(two gN pN,pSubmitR)]    $ find (two pN sRN) g
  assertEqual [(two gN pN,pTrainFileR)] $ find (two pN tfRN) g
  assertEqual [(two gN pN,pTrainRunR)]  $ find (two pN trRN) g
  assertEqual [(two pN trRN,tr)]        $ find (two trRN trN) p
  
  assertEqual [(three rootN rN tN,c)]        $ find (three rN tN cN) root
  assertEqual [(three rN tN cN,g)]           $ find (three tN cN gN) r
  assertEqual [(three tN cN gN,p)]           $ find (three cN gN pN) t
  assertEqual [(three cN gN pN,pSubmitR)]    $ find (three gN pN sRN) c
  assertEqual [(three cN gN pN,pTrainFileR)] $ find (three gN pN tfRN) c
  assertEqual [(three cN gN pN,pTrainRunR)]  $ find (three gN pN trRN) c
  assertEqual [(three gN pN trRN,tr)]        $ find (three pN trRN trN) g
  
  assertEqual [(four rootN rN tN cN,g)]        $ find (four rN tN cN gN) root
  assertEqual [(four rN tN cN gN,p)]           $ find (four tN cN gN pN) r
  assertEqual [(four tN cN gN pN,pSubmitR)]    $ find (four cN gN pN sRN) t
  assertEqual [(four tN cN gN pN,pTrainFileR)] $ find (four cN gN pN tfRN) t
  assertEqual [(four tN cN gN pN,pTrainRunR)]  $ find (four cN gN pN trRN) t
  assertEqual [(four cN gN pN trRN,tr)]        $ find (four gN pN trRN trN) c
  
  assertEqual [(five rootN rN tN cN gN,p)]        $ find (five rN tN cN gN pN) root
  assertEqual [(five rN tN cN gN pN,pSubmitR)]    $ find (five tN cN gN pN sRN) r
  assertEqual [(five rN tN cN gN pN,pTrainFileR)] $ find (five tN cN gN pN tfRN) r
  assertEqual [(five rN tN cN gN pN,pTrainRunR)]  $ find (five tN cN gN pN trRN) r
  assertEqual [(five tN cN gN pN trRN, tr)]             $ find (five cN gN pN trRN trN) t
  
  assertEqual [(six rootN rN tN cN gN pN,pSubmitR)]    $ find (six rN tN cN gN pN sRN) root
  assertEqual [(six rootN rN tN cN gN pN,pTrainFileR)] $ find (six rN tN cN gN pN tfRN) root
  assertEqual [(six rootN rN tN cN gN pN,pTrainRunR)]  $ find (six rN tN cN gN pN trRN) root
  assertEqual [(six rN tN cN gN pN trRN,tr)]          $ find (six tN cN gN pN trRN trN) r
  
  assertEqual [(seven rootN rN tN cN gN pN trRN,tr)]          $ find (seven rN tN cN gN pN trRN trN) root

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


