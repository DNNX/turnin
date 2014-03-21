{-# OPTIONS_GHC -F -pgmF htfpp -fno-warn-incomplete-patterns#-}
module Infrastructure.Finder.FinderUnitFindingTest where

import Test.Framework
import Infrastructure.Finder.FinderTestUtils

import Infrastructure.Node
import Domain.Root
import Domain.Project
import Domain.ProjectRepo

{-# ANN module "HLint: ignore Use camelCase" #-}
{-# ANN module "HLint: ignore Reduce duplication" #-}
{-# ANN module "HLint: ignore Evaluate" #-}
{-# ANN module "HLint: ignore Use ++" #-}

test_zero = do
  assertState (zero find' root ldRoot) rootC [(zeroK,root)]
  assertState (zero find' r    ldR)    rC    [(zeroK,r)]
  assertState (zero find' t    ldT)    tC    [(zeroK,t)]
  assertState (zero find' c    ldC)    cC    [(zeroK,c)]
  assertState (zero find' g    ldG)    gC    [(zeroK,g)]
  assertState (zero find' p    ldP)    pC    [(zeroK,p)]
  assertState (zero find' pSr  ldPr)   prC   [(zeroK,pSr)]
  assertState (zero find' pTfr ldPr)   prC   [(zeroK,pTfr)]
  assertState (zero find' pTrr ldPr)   prC   [(zeroK,pTrr)]
  assertState (zero find' tr   ldTr)   trC   [(zeroK,tr)]

  assertState (zero findUnambiguous' root ldRoot) rootC (Just root)
  assertState (zero findUnambiguous' r    ldR)    rC    (Just r)
  assertState (zero findUnambiguous' t    ldT)    tC    (Just t)
  assertState (zero findUnambiguous' c    ldC)    cC    (Just c)
  assertState (zero findUnambiguous' g    ldG)    gC    (Just g)
  assertState (zero findUnambiguous' p    ldP)    pC    (Just p)
  assertState (zero findUnambiguous' pSr  ldPr)   prC   (Just pSr)
  assertState (zero findUnambiguous' pTfr ldPr)   prC   (Just pTfr)
  assertState (zero findUnambiguous' pTrr ldPr)   prC   (Just pTrr)
  assertState (zero findUnambiguous' tr   ldTr)   trC   (Just tr)

test_one = do
  assertState (one' find' root ldRoot ldR)  (rootC|+rC) [(oneK rootN,r)]
  assertState (one' find' r    ldR    ldT)  (rC|+tC)    [(oneK rN,t)]
  assertState (one' find' t    ldT    ldC)  (tC|+cC)    [(oneK tN,c)]
  assertState (one' find' c    ldC    ldG)  (cC|+gC)    [(oneK cN,g)]
  assertState (one' find' g    ldG    ldP)  (gC|+pC)    [(oneK gN,p)]
  assertState (one' find' p    ldP    ldPr) (pC|+pr3C)  (prs $ oneK pN)
  assertState (one' find' pTrr ldPr   ldTr) (prC|+trC)  [(oneK trRN,tr)]

  assertState (one' findUnambiguous' root ldRoot ldR)  (rootC|+rC) (Just r)
  assertState (one' findUnambiguous' r    ldR    ldT)  (rC|+tC)    (Just t)
  assertState (one' findUnambiguous' t    ldT    ldC)  (tC|+cC)    (Just c)
  assertState (one' findUnambiguous' c    ldC    ldG)  (cC|+gC)    (Just g)
  assertState (one' findUnambiguous' g    ldG    ldP)  (gC|+pC)    (Just p)
  assertState (one' findUnambiguous' pTrr ldPr   ldTr) (prC|+trC)  (Just tr)

  assertState (one rN   find' root ldRoot ldR)  (rootC|+rC) [(oneK rootN,r)]
  assertState (one tN   find' r    ldR    ldT)  (rC|+tC)    [(oneK rN,t)]
  assertState (one cN   find' t    ldT    ldC)  (tC|+cC)    [(oneK tN,c)]
  assertState (one gN   find' c    ldC    ldG)  (cC|+gC)    [(oneK cN,g)]
  assertState (one pN   find' g    ldG    ldP)  (gC|+pC)    [(oneK gN,p)]
  assertState (one sRN  find' p    ldP    ldPr) (pC|+prC)   [(oneK pN,pSr)]
  assertState (one tfRN find' p    ldP    ldPr) (pC|+prC)   [(oneK pN,pTfr)]
  assertState (one trRN find' p    ldP    ldPr) (pC|+prC)   [(oneK pN,pTrr)]
  assertState (one trN  find' pTrr ldPr   ldTr) (prC|+trC)  [(oneK trRN,tr)]

  assertState (one rN   findUnambiguous' root ldRoot ldR)  (rootC|+rC) (Just r)
  assertState (one tN   findUnambiguous' r    ldR    ldT)  (rC|+tC)    (Just t)
  assertState (one cN   findUnambiguous' t    ldT    ldC)  (tC|+cC)    (Just c)
  assertState (one gN   findUnambiguous' c    ldC    ldG)  (cC|+gC)    (Just g)
  assertState (one pN   findUnambiguous' g    ldG    ldP)  (gC|+pC)    (Just p)
  assertState (one sRN  findUnambiguous' p    ldP    ldPr) (pC|+prC)   (Just pSr)
  assertState (one tfRN findUnambiguous' p    ldP    ldPr) (pC|+prC)   (Just pTfr)
  assertState (one trRN findUnambiguous' p    ldP    ldPr) (pC|+prC)   (Just pTrr)
  assertState (one trN  findUnambiguous' pTrr ldPr   ldTr) (prC|+trC)  (Just tr)

test_two = do
  assertState (two' find' root ldRoot ldR  ldT)  (rootC|+rC|+tC) [(twoK rootN rN,t)]
  assertState (two' find' r    ldR    ldT  ldC)  (rC|+tC|+cC)    [(twoK rN tN,c)]
  assertState (two' find' t    ldT    ldC  ldG)  (tC|+cC|+gC)    [(twoK tN cN,g)]
  assertState (two' find' c    ldC    ldG  ldP)  (cC|+gC|+pC)    [(twoK cN gN,p)]
  assertState (two' find' g    ldG    ldP  ldPr) (gC|+pC|+pr3C)  (prs $ twoK gN pN)
  assertState (two' find' p    ldP    ldPr ldTr) (pC|+pr3C|+trC) [(twoK pN trRN,tr)]

  assertState (two' findUnambiguous' root ldRoot ldR  ldT)  (rootC|+rC|+tC) (Just t)
  assertState (two' findUnambiguous' r    ldR    ldT  ldC)  (rC|+tC|+cC)    (Just c)
  assertState (two' findUnambiguous' t    ldT    ldC  ldG)  (tC|+cC|+gC)    (Just g)
  assertState (two' findUnambiguous' c    ldC    ldG  ldP)  (cC|+gC|+pC)    (Just p)
  assertState (two' findUnambiguous' p    ldP    ldPr ldTr) (pC|+pr3C|+trC) (Just tr)

  assertState (two rN   tN   find' root ldRoot ldR  ldT)  (rootC|+rC|+tC) [(twoK rootN rN,t)]
  assertState (two tN   cN   find' r    ldR    ldT  ldC)  (rC|+tC|+cC)    [(twoK rN tN,c)]
  assertState (two cN   gN   find' t    ldT    ldC  ldG)  (tC|+cC|+gC)    [(twoK tN cN,g)]
  assertState (two gN   pN   find' c    ldC    ldG  ldP)  (cC|+gC|+pC)    [(twoK cN gN,p)]
  assertState (two pN   sRN  find' g    ldG    ldP  ldPr) (gC|+pC|+prC)   [(twoK gN pN,pSr)]
  assertState (two pN   tfRN find' g    ldG    ldP  ldPr) (gC|+pC|+prC)   [(twoK gN pN,pTfr)]
  assertState (two pN   trRN find' g    ldG    ldP  ldPr) (gC|+pC|+prC)   [(twoK gN pN,pTrr)]
  assertState (two trRN trN  find' p    ldP    ldPr ldTr) (pC|+prC|+trC)  [(twoK pN trRN,tr)]

  assertState (two rN   tN   findUnambiguous' root ldRoot ldR  ldT)  (rootC|+rC|+tC) (Just t)
  assertState (two tN   cN   findUnambiguous' r    ldR    ldT  ldC)  (rC|+tC|+cC)    (Just c)
  assertState (two cN   gN   findUnambiguous' t    ldT    ldC  ldG)  (tC|+cC|+gC)    (Just g)
  assertState (two gN   pN   findUnambiguous' c    ldC    ldG  ldP)  (cC|+gC|+pC)    (Just p)
  assertState (two pN   sRN  findUnambiguous' g    ldG    ldP  ldPr) (gC|+pC|+prC)   (Just pSr)
  assertState (two pN   tfRN findUnambiguous' g    ldG    ldP  ldPr) (gC|+pC|+prC)   (Just pTfr)
  assertState (two pN   trRN findUnambiguous' g    ldG    ldP  ldPr) (gC|+pC|+prC)   (Just pTrr)
  assertState (two trRN trN  findUnambiguous' p    ldP    ldPr ldTr) (pC|+prC|+trC)  (Just tr)

test_three = do
  assertState (three' find' root ldRoot ldR ldT  ldC)  (rootC|+rC|+tC|+cC) [(threeK rootN rN tN,c)]
  assertState (three' find' r    ldR    ldT ldC  ldG)  (rC|+tC|+cC|+gC)    [(threeK rN tN cN,g)]
  assertState (three' find' t    ldT    ldC ldG  ldP)  (tC|+cC|+gC|+pC)    [(threeK tN cN gN,p)]
  assertState (three' find' c    ldC    ldG ldP  ldPr) (cC|+gC|+pC|+pr3C)  (prs $ threeK cN gN pN)
  assertState (three' find' g    ldG    ldP ldPr ldTr) (gC|+pC|+pr3C|+trC) [(threeK gN pN trRN,tr)]

  assertState (three' findUnambiguous' root ldRoot ldR ldT  ldC)  (rootC|+rC|+tC|+cC) (Just c)
  assertState (three' findUnambiguous' r    ldR    ldT ldC  ldG)  (rC|+tC|+cC|+gC)    (Just g)
  assertState (three' findUnambiguous' t    ldT    ldC ldG  ldP)  (tC|+cC|+gC|+pC)    (Just p)
  assertState (three' findUnambiguous' g    ldG    ldP ldPr ldTr) (gC|+pC|+pr3C|+trC) (Just tr)
  
  assertState (three rN tN   cN   find' root ldRoot ldR ldT  ldC)  (rootC|+rC|+tC|+cC) [(threeK rootN rN tN,c)]
  assertState (three tN cN   gN   find' r    ldR    ldT ldC  ldG)  (rC|+tC|+cC|+gC)    [(threeK rN tN cN,g)]
  assertState (three cN gN   pN   find' t    ldT    ldC ldG  ldP)  (tC|+cC|+gC|+pC)    [(threeK tN cN gN,p)]
  assertState (three gN pN   sRN  find' c    ldC    ldG ldP  ldPr) (cC|+gC|+pC|+prC)   [(threeK cN gN pN,pSr)]
  assertState (three gN pN   tfRN find' c    ldC    ldG ldP  ldPr) (cC|+gC|+pC|+prC)   [(threeK cN gN pN,pTfr)]
  assertState (three gN pN   trRN find' c    ldC    ldG ldP  ldPr) (cC|+gC|+pC|+prC)   [(threeK cN gN pN,pTrr)]
  assertState (three pN trRN trN  find' g    ldG    ldP ldPr ldTr) (gC|+pC|+prC|+trC)  [(threeK gN pN trRN,tr)]
                     
  assertState (three rN tN   cN   findUnambiguous' root ldRoot ldR ldT  ldC)  (rootC|+rC|+tC|+cC) (Just c)
  assertState (three tN cN   gN   findUnambiguous' r    ldR    ldT ldC  ldG)  (rC|+tC|+cC|+gC)    (Just g)
  assertState (three cN gN   pN   findUnambiguous' t    ldT    ldC ldG  ldP)  (tC|+cC|+gC|+pC)    (Just p)
  assertState (three gN pN   sRN  findUnambiguous' c    ldC    ldG ldP  ldPr) (cC|+gC|+pC|+prC)   (Just pSr)
  assertState (three gN pN   tfRN findUnambiguous' c    ldC    ldG ldP  ldPr) (cC|+gC|+pC|+prC)   (Just pTfr)
  assertState (three gN pN   trRN findUnambiguous' c    ldC    ldG ldP  ldPr) (cC|+gC|+pC|+prC)   (Just pTrr)
  assertState (three pN trRN trN  findUnambiguous' g    ldG    ldP ldPr ldTr) (gC|+pC|+prC|+trC)  (Just tr)

test_four = do
  assertState (four' find' root ldRoot ldR ldT ldC  ldG)  (rootC|+rC|+tC|+cC|+gC) [(fourK rootN rN tN cN,g)]
  assertState (four' find' r    ldR    ldT ldC ldG  ldP)  (rC|+tC|+cC|+gC|+pC)    [(fourK rN tN cN gN,p)]
  assertState (four' find' t    ldT    ldC ldG ldP  ldPr) (tC|+cC|+gC|+pC|+pr3C)  (prs $ fourK tN cN gN pN)
  assertState (four' find' c    ldC    ldG ldP ldPr ldTr) (cC|+gC|+pC|+pr3C|+trC) [(fourK cN gN pN trRN,tr)]
  
  assertState (four' findUnambiguous' root ldRoot ldR ldT ldC  ldG)  (rootC|+rC|+tC|+cC|+gC) (Just g) 
  assertState (four' findUnambiguous' r    ldR    ldT ldC ldG  ldP)  (rC|+tC|+cC|+gC|+pC)    (Just p)
  assertState (four' findUnambiguous' c    ldC    ldG ldP ldPr ldTr) (cC|+gC|+pC|+pr3C|+trC) (Just tr)
  
  assertState (four rN tN cN   gN   find' root ldRoot ldR ldT ldC  ldG)  (rootC|+rC|+tC|+cC|+gC) [(fourK rootN rN tN cN,g)]
  assertState (four tN cN gN   pN   find' r    ldR    ldT ldC ldG  ldP)  (rC|+tC|+cC|+gC|+pC)    [(fourK rN tN cN gN,p)]
  assertState (four cN gN pN   sRN  find' t    ldT    ldC ldG ldP  ldPr) (tC|+cC|+gC|+pC|+prC)   [(fourK tN cN gN pN,pSr)]
  assertState (four cN gN pN   tfRN find' t    ldT    ldC ldG ldP  ldPr) (tC|+cC|+gC|+pC|+prC)   [(fourK tN cN gN pN,pTfr)]
  assertState (four cN gN pN   trRN find' t    ldT    ldC ldG ldP  ldPr) (tC|+cC|+gC|+pC|+prC)   [(fourK tN cN gN pN,pTrr)]
  assertState (four gN pN trRN trN  find' c    ldC    ldG ldP ldPr ldTr) (cC|+gC|+pC|+prC|+trC)  [(fourK cN gN pN trRN,tr)]
                    
  assertState (four rN tN cN   gN   findUnambiguous' root ldRoot ldR ldT ldC  ldG)  (rootC|+rC|+tC|+cC|+gC) (Just g) 
  assertState (four tN cN gN   pN   findUnambiguous' r    ldR    ldT ldC ldG  ldP)  (rC|+tC|+cC|+gC|+pC)    (Just p)
  assertState (four cN gN pN   sRN  findUnambiguous' t    ldT    ldC ldG ldP  ldPr) (tC|+cC|+gC|+pC|+prC)   (Just pSr)
  assertState (four cN gN pN   tfRN findUnambiguous' t    ldT    ldC ldG ldP  ldPr) (tC|+cC|+gC|+pC|+prC)   (Just pTfr)
  assertState (four cN gN pN   trRN findUnambiguous' t    ldT    ldC ldG ldP  ldPr) (tC|+cC|+gC|+pC|+prC)   (Just pTrr)
  assertState (four gN pN trRN trN  findUnambiguous' c    ldC    ldG ldP ldPr ldTr) (cC|+gC|+pC|+prC|+trC)  (Just tr)
  
test_five = do
  assertState (five' find' root ldRoot ldR ldT ldC ldG  ldP)  (rootC|+rC|+tC|+cC|+gC|+pC) [(fiveK rootN rN tN cN gN,p)]
  assertState (five' find' r    ldR    ldT ldC ldG ldP  ldPr) (rC|+tC|+cC|+gC|+pC|+pr3C)  (prs $ fiveK rN tN cN gN pN)
  assertState (five' find' t    ldT    ldC ldG ldP ldPr ldTr) (tC|+cC|+gC|+pC|+pr3C|+trC) [(fiveK tN cN gN pN trRN,tr)]
  
  assertState (five' findUnambiguous' root ldRoot ldR ldT ldC ldG  ldP)  (rootC|+rC|+tC|+cC|+gC|+pC) (Just p) 
  assertState (five' findUnambiguous' t    ldT    ldC ldG ldP ldPr ldTr) (tC|+cC|+gC|+pC|+pr3C|+trC) (Just tr)
  
  assertState (five rN tN cN gN   pN   find' root ldRoot ldR ldT ldC ldG  ldP)  (rootC|+rC|+tC|+cC|+gC|+pC) [(fiveK rootN rN tN cN gN,p)]
  assertState (five tN cN gN pN   sRN  find' r    ldR    ldT ldC ldG ldP  ldPr) (rC|+tC|+cC|+gC|+pC|+prC)   [(fiveK rN tN cN gN pN,pSr)]
  assertState (five tN cN gN pN   tfRN find' r    ldR    ldT ldC ldG ldP  ldPr) (rC|+tC|+cC|+gC|+pC|+prC)   [(fiveK rN tN cN gN pN,pTfr)]
  assertState (five tN cN gN pN   trRN find' r    ldR    ldT ldC ldG ldP  ldPr) (rC|+tC|+cC|+gC|+pC|+prC)   [(fiveK rN tN cN gN pN,pTrr)]
  assertState (five cN gN pN trRN trN  find' t    ldT    ldC ldG ldP ldPr ldTr) (tC|+cC|+gC|+pC|+prC|+trC)  [(fiveK tN cN gN pN trRN,tr)]
  
  assertState (five rN tN cN gN   pN   findUnambiguous' root ldRoot ldR ldT ldC ldG  ldP)  (rootC|+rC|+tC|+cC|+gC|+pC) (Just p) 
  assertState (five tN cN gN pN   sRN  findUnambiguous' r    ldR    ldT ldC ldG ldP  ldPr) (rC|+tC|+cC|+gC|+pC|+prC)   (Just pSr)
  assertState (five tN cN gN pN   tfRN findUnambiguous' r    ldR    ldT ldC ldG ldP  ldPr) (rC|+tC|+cC|+gC|+pC|+prC)   (Just pTfr)
  assertState (five tN cN gN pN   trRN findUnambiguous' r    ldR    ldT ldC ldG ldP  ldPr) (rC|+tC|+cC|+gC|+pC|+prC)   (Just pTrr)
  assertState (five cN gN pN trRN trN  findUnambiguous' t    ldT    ldC ldG ldP ldPr ldTr) (tC|+cC|+gC|+pC|+prC|+trC)  (Just tr)
   
test_six = do
  assertState (six' find' root ldRoot ldR ldT ldC ldG ldP  ldPr) (rootC|+rC|+tC|+cC|+gC|+pC|+pr3C) (prs $ sixK rootN rN tN cN gN pN)
  assertState (six' find' r    ldR    ldT ldC ldG ldP ldPr ldTr) (rC|+tC|+cC|+gC|+pC|+pr3C|+trC)   [(sixK rN tN cN gN pN trRN,tr)]
  
  assertState (six' findUnambiguous' r    ldR    ldT ldC ldG ldP ldPr ldTr) (rC|+tC|+cC|+gC|+pC|+pr3C|+trC) (Just tr)
  
  assertState (six rN tN cN gN pN   sRN  find' root ldRoot ldR ldT ldC ldG ldP  ldPr) (rootC|+rC|+tC|+cC|+gC|+pC|+prC) [(sixK rootN rN tN cN gN pN,pSr)]
  assertState (six rN tN cN gN pN   tfRN find' root ldRoot ldR ldT ldC ldG ldP  ldPr) (rootC|+rC|+tC|+cC|+gC|+pC|+prC) [(sixK rootN rN tN cN gN pN,pTfr)]
  assertState (six rN tN cN gN pN   trRN find' root ldRoot ldR ldT ldC ldG ldP  ldPr) (rootC|+rC|+tC|+cC|+gC|+pC|+prC) [(sixK rootN rN tN cN gN pN,pTrr)]
  assertState (six tN cN gN pN trRN trN  find' r    ldR    ldT ldC ldG ldP ldPr ldTr) (rC|+tC|+cC|+gC|+pC|+prC|+trC)   [(sixK rN tN cN gN pN trRN,tr)]
  
  assertState (six rN tN cN gN pN   sRN  findUnambiguous' root ldRoot ldR ldT ldC ldG ldP  ldPr) (rootC|+rC|+tC|+cC|+gC|+pC|+prC) (Just pSr)
  assertState (six rN tN cN gN pN   tfRN findUnambiguous' root ldRoot ldR ldT ldC ldG ldP  ldPr) (rootC|+rC|+tC|+cC|+gC|+pC|+prC) (Just pTfr)
  assertState (six rN tN cN gN pN   trRN findUnambiguous' root ldRoot ldR ldT ldC ldG ldP  ldPr) (rootC|+rC|+tC|+cC|+gC|+pC|+prC) (Just pTrr)
  assertState (six tN cN gN pN trRN trN  findUnambiguous' r    ldR    ldT ldC ldG ldP ldPr ldTr) (rC|+tC|+cC|+gC|+pC|+prC|+trC)   (Just tr)
  
test_seven = do
  assertState (seven' find'            root ldRoot ldR ldT ldC ldG ldP ldPr ldTr) (rootC|+rC|+tC|+cC|+gC|+pC|+pr3C|+trC) [(sevenK rootN rN tN cN gN pN trRN,tr)]
  assertState (seven' findUnambiguous' root ldRoot ldR ldT ldC ldG ldP ldPr ldTr) (rootC|+rC|+tC|+cC|+gC|+pC|+pr3C|+trC) (Just tr)
  
  assertState (seven rN tN cN gN pN trRN trN find'            root ldRoot ldR ldT ldC ldG ldP ldPr ldTr) (rootC|+rC|+tC|+cC|+gC|+pC|+prC|+trC) [(sevenK rootN rN tN cN gN pN trRN,tr)]
  assertState (seven rN tN cN gN pN trRN trN findUnambiguous' root ldRoot ldR ldT ldC ldG ldP ldPr ldTr) (rootC|+rC|+tC|+cC|+gC|+pC|+prC|+trC) (Just tr)
    
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

tr = make trN
p = setTrainRunRepo (make pN) trr
g = addChild (make gN) p
c = addChild (make cN) g
t = addChild (make tN) c
r = addChild (make rN) t
root = addChild (make rootN :: Root) r

sr = emptySubmitRepo
tfr = emptyTrainFileRepo
trr = addChild emptyTrainRunRepo tr

pSr = makeProjectSubmitRepo sr
pTfr = makeProjectTrainFileRepo tfr
pTrr = makeProjectTrainRunRepo trr

prs k = [(k,pSr),(k,pTfr),(k,pTrr)]
  