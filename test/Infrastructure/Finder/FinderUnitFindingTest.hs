{-# OPTIONS_GHC -F -pgmF htfpp -fno-warn-incomplete-patterns#-}
module Infrastructure.Finder.FinderUnitFindingTest where

import Test.Framework
import Infrastructure.Finder.FinderTestUtils

{-# ANN module "HLint: ignore Use camelCase" #-}
{-# ANN module "HLint: ignore Reduce duplication" #-}
{-# ANN module "HLint: ignore Evaluate" #-}
{-# ANN module "HLint: ignore Use ++" #-}

test_zero = do
  assertState (zero find' root ldRoot) rootCall [(zeroK,root)]
  assertState (zero find' r    ldR)    rCall    [(zeroK,r)]
  assertState (zero find' t    ldT)    tCall    [(zeroK,t)]
  assertState (zero find' c    ldC)    cCall    [(zeroK,c)]
  assertState (zero find' g    ldG)    gCall    [(zeroK,g)]
  assertState (zero find' p    ldP)    pCall    [(zeroK,p)]
  assertState (zero find' pSr  ldPr)   prCall   [(zeroK,pSr)]
  assertState (zero find' pTfr ldPr)   prCall   [(zeroK,pTfr)]
  assertState (zero find' pTrr ldPr)   prCall   [(zeroK,pTrr)]
  assertState (zero find' tr   ldTr)   trCall   [(zeroK,tr)]

  assertState (zero findUnambiguous' root ldRoot) rootCall (Just root)
  assertState (zero findUnambiguous' r    ldR)    rCall    (Just r)
  assertState (zero findUnambiguous' t    ldT)    tCall    (Just t)
  assertState (zero findUnambiguous' c    ldC)    cCall    (Just c)
  assertState (zero findUnambiguous' g    ldG)    gCall    (Just g)
  assertState (zero findUnambiguous' p    ldP)    pCall    (Just p)
  assertState (zero findUnambiguous' pSr  ldPr)   prCall   (Just pSr)
  assertState (zero findUnambiguous' pTfr ldPr)   prCall   (Just pTfr)
  assertState (zero findUnambiguous' pTrr ldPr)   prCall   (Just pTrr)
  assertState (zero findUnambiguous' tr   ldTr)   trCall   (Just tr)

test_one = do
  assertState (one' find' root ldRoot ldR)  (rootCall|+rCall) [(oneK rootN,r)]
  assertState (one' find' r    ldR    ldT)  (rCall|+tCall)    [(oneK rN,t)]
  assertState (one' find' t    ldT    ldC)  (tCall|+cCall)    [(oneK tN,c)]
  assertState (one' find' c    ldC    ldG)  (cCall|+gCall)    [(oneK cN,g)]
  assertState (one' find' g    ldG    ldP)  (gCall|+pCall)    [(oneK gN,p)]
  assertState (one' find' p    ldP    ldPr) (pCall|+pr3Call)  (prs $ oneK pN)
  assertState (one' find' pTrr ldPr   ldTr) (prCall|+trCall)  [(oneK trRN,tr)]

  assertState (one' findUnambiguous' root ldRoot ldR)  (rootCall|+rCall) (Just r)
  assertState (one' findUnambiguous' r    ldR    ldT)  (rCall|+tCall)    (Just t)
  assertState (one' findUnambiguous' t    ldT    ldC)  (tCall|+cCall)    (Just c)
  assertState (one' findUnambiguous' c    ldC    ldG)  (cCall|+gCall)    (Just g)
  assertState (one' findUnambiguous' g    ldG    ldP)  (gCall|+pCall)    (Just p)
  assertState (one' findUnambiguous' pTrr ldPr   ldTr) (prCall|+trCall)  (Just tr)

  assertState (one rN   find' root ldRoot ldR)  (rootCall|+rCall) [(oneK rootN,r)]
  assertState (one tN   find' r    ldR    ldT)  (rCall|+tCall)    [(oneK rN,t)]
  assertState (one cN   find' t    ldT    ldC)  (tCall|+cCall)    [(oneK tN,c)]
  assertState (one gN   find' c    ldC    ldG)  (cCall|+gCall)    [(oneK cN,g)]
  assertState (one pN   find' g    ldG    ldP)  (gCall|+pCall)    [(oneK gN,p)]
  assertState (one sRN  find' p    ldP    ldPr) (pCall|+prCall)   [(oneK pN,pSr)]
  assertState (one tfRN find' p    ldP    ldPr) (pCall|+prCall)   [(oneK pN,pTfr)]
  assertState (one trRN find' p    ldP    ldPr) (pCall|+prCall)   [(oneK pN,pTrr)]
  assertState (one trN  find' pTrr ldPr   ldTr) (prCall|+trCall)  [(oneK trRN,tr)]

  assertState (one rN   findUnambiguous' root ldRoot ldR)  (rootCall|+rCall) (Just r)
  assertState (one tN   findUnambiguous' r    ldR    ldT)  (rCall|+tCall)    (Just t)
  assertState (one cN   findUnambiguous' t    ldT    ldC)  (tCall|+cCall)    (Just c)
  assertState (one gN   findUnambiguous' c    ldC    ldG)  (cCall|+gCall)    (Just g)
  assertState (one pN   findUnambiguous' g    ldG    ldP)  (gCall|+pCall)    (Just p)
  assertState (one sRN  findUnambiguous' p    ldP    ldPr) (pCall|+prCall)   (Just pSr)
  assertState (one tfRN findUnambiguous' p    ldP    ldPr) (pCall|+prCall)   (Just pTfr)
  assertState (one trRN findUnambiguous' p    ldP    ldPr) (pCall|+prCall)   (Just pTrr)
  assertState (one trN  findUnambiguous' pTrr ldPr   ldTr) (prCall|+trCall)  (Just tr)

test_two = do
  assertState (two' find' root ldRoot ldR  ldT)  (rootCall|+rCall|+tCall) [(twoK rootN rN,t)]
  assertState (two' find' r    ldR    ldT  ldC)  (rCall|+tCall|+cCall)    [(twoK rN tN,c)]
  assertState (two' find' t    ldT    ldC  ldG)  (tCall|+cCall|+gCall)    [(twoK tN cN,g)]
  assertState (two' find' c    ldC    ldG  ldP)  (cCall|+gCall|+pCall)    [(twoK cN gN,p)]
  assertState (two' find' g    ldG    ldP  ldPr) (gCall|+pCall|+pr3Call)  (prs $ twoK gN pN)
  assertState (two' find' p    ldP    ldPr ldTr) (pCall|+pr3Call|+trCall) [(twoK pN trRN,tr)]

  assertState (two' findUnambiguous' root ldRoot ldR  ldT)  (rootCall|+rCall|+tCall) (Just t)
  assertState (two' findUnambiguous' r    ldR    ldT  ldC)  (rCall|+tCall|+cCall)    (Just c)
  assertState (two' findUnambiguous' t    ldT    ldC  ldG)  (tCall|+cCall|+gCall)    (Just g)
  assertState (two' findUnambiguous' c    ldC    ldG  ldP)  (cCall|+gCall|+pCall)    (Just p)
  assertState (two' findUnambiguous' p    ldP    ldPr ldTr) (pCall|+pr3Call|+trCall) (Just tr)

  assertState (two rN   tN   find' root ldRoot ldR  ldT)  (rootCall|+rCall|+tCall) [(twoK rootN rN,t)]
  assertState (two tN   cN   find' r    ldR    ldT  ldC)  (rCall|+tCall|+cCall)    [(twoK rN tN,c)]
  assertState (two cN   gN   find' t    ldT    ldC  ldG)  (tCall|+cCall|+gCall)    [(twoK tN cN,g)]
  assertState (two gN   pN   find' c    ldC    ldG  ldP)  (cCall|+gCall|+pCall)    [(twoK cN gN,p)]
  assertState (two pN   sRN  find' g    ldG    ldP  ldPr) (gCall|+pCall|+prCall)   [(twoK gN pN,pSr)]
  assertState (two pN   tfRN find' g    ldG    ldP  ldPr) (gCall|+pCall|+prCall)   [(twoK gN pN,pTfr)]
  assertState (two pN   trRN find' g    ldG    ldP  ldPr) (gCall|+pCall|+prCall)   [(twoK gN pN,pTrr)]
  assertState (two trRN trN  find' p    ldP    ldPr ldTr) (pCall|+prCall|+trCall)  [(twoK pN trRN,tr)]

  assertState (two rN   tN   findUnambiguous' root ldRoot ldR  ldT)  (rootCall|+rCall|+tCall) (Just t)
  assertState (two tN   cN   findUnambiguous' r    ldR    ldT  ldC)  (rCall|+tCall|+cCall)    (Just c)
  assertState (two cN   gN   findUnambiguous' t    ldT    ldC  ldG)  (tCall|+cCall|+gCall)    (Just g)
  assertState (two gN   pN   findUnambiguous' c    ldC    ldG  ldP)  (cCall|+gCall|+pCall)    (Just p)
  assertState (two pN   sRN  findUnambiguous' g    ldG    ldP  ldPr) (gCall|+pCall|+prCall)   (Just pSr)
  assertState (two pN   tfRN findUnambiguous' g    ldG    ldP  ldPr) (gCall|+pCall|+prCall)   (Just pTfr)
  assertState (two pN   trRN findUnambiguous' g    ldG    ldP  ldPr) (gCall|+pCall|+prCall)   (Just pTrr)
  assertState (two trRN trN  findUnambiguous' p    ldP    ldPr ldTr) (pCall|+prCall|+trCall)  (Just tr)

test_three = do
  assertState (three' find' root ldRoot ldR ldT  ldC)  (rootCall|+rCall|+tCall|+cCall) [(threeK rootN rN tN,c)]
  assertState (three' find' r    ldR    ldT ldC  ldG)  (rCall|+tCall|+cCall|+gCall)    [(threeK rN tN cN,g)]
  assertState (three' find' t    ldT    ldC ldG  ldP)  (tCall|+cCall|+gCall|+pCall)    [(threeK tN cN gN,p)]
  assertState (three' find' c    ldC    ldG ldP  ldPr) (cCall|+gCall|+pCall|+pr3Call)  (prs $ threeK cN gN pN)
  assertState (three' find' g    ldG    ldP ldPr ldTr) (gCall|+pCall|+pr3Call|+trCall) [(threeK gN pN trRN,tr)]

  assertState (three' findUnambiguous' root ldRoot ldR ldT  ldC)  (rootCall|+rCall|+tCall|+cCall) (Just c)
  assertState (three' findUnambiguous' r    ldR    ldT ldC  ldG)  (rCall|+tCall|+cCall|+gCall)    (Just g)
  assertState (three' findUnambiguous' t    ldT    ldC ldG  ldP)  (tCall|+cCall|+gCall|+pCall)    (Just p)
  assertState (three' findUnambiguous' g    ldG    ldP ldPr ldTr) (gCall|+pCall|+pr3Call|+trCall) (Just tr)
  
  assertState (three rN tN   cN   find' root ldRoot ldR ldT  ldC)  (rootCall|+rCall|+tCall|+cCall) [(threeK rootN rN tN,c)]
  assertState (three tN cN   gN   find' r    ldR    ldT ldC  ldG)  (rCall|+tCall|+cCall|+gCall)    [(threeK rN tN cN,g)]
  assertState (three cN gN   pN   find' t    ldT    ldC ldG  ldP)  (tCall|+cCall|+gCall|+pCall)    [(threeK tN cN gN,p)]
  assertState (three gN pN   sRN  find' c    ldC    ldG ldP  ldPr) (cCall|+gCall|+pCall|+prCall)   [(threeK cN gN pN,pSr)]
  assertState (three gN pN   tfRN find' c    ldC    ldG ldP  ldPr) (cCall|+gCall|+pCall|+prCall)   [(threeK cN gN pN,pTfr)]
  assertState (three gN pN   trRN find' c    ldC    ldG ldP  ldPr) (cCall|+gCall|+pCall|+prCall)   [(threeK cN gN pN,pTrr)]
  assertState (three pN trRN trN  find' g    ldG    ldP ldPr ldTr) (gCall|+pCall|+prCall|+trCall)  [(threeK gN pN trRN,tr)]
                     
  assertState (three rN tN   cN   findUnambiguous' root ldRoot ldR ldT  ldC)  (rootCall|+rCall|+tCall|+cCall) (Just c)
  assertState (three tN cN   gN   findUnambiguous' r    ldR    ldT ldC  ldG)  (rCall|+tCall|+cCall|+gCall)    (Just g)
  assertState (three cN gN   pN   findUnambiguous' t    ldT    ldC ldG  ldP)  (tCall|+cCall|+gCall|+pCall)    (Just p)
  assertState (three gN pN   sRN  findUnambiguous' c    ldC    ldG ldP  ldPr) (cCall|+gCall|+pCall|+prCall)   (Just pSr)
  assertState (three gN pN   tfRN findUnambiguous' c    ldC    ldG ldP  ldPr) (cCall|+gCall|+pCall|+prCall)   (Just pTfr)
  assertState (three gN pN   trRN findUnambiguous' c    ldC    ldG ldP  ldPr) (cCall|+gCall|+pCall|+prCall)   (Just pTrr)
  assertState (three pN trRN trN  findUnambiguous' g    ldG    ldP ldPr ldTr) (gCall|+pCall|+prCall|+trCall)  (Just tr)

test_four = do
  assertState (four' find' root ldRoot ldR ldT ldC  ldG)  (rootCall|+rCall|+tCall|+cCall|+gCall) [(fourK rootN rN tN cN,g)]
  assertState (four' find' r    ldR    ldT ldC ldG  ldP)  (rCall|+tCall|+cCall|+gCall|+pCall)    [(fourK rN tN cN gN,p)]
  assertState (four' find' t    ldT    ldC ldG ldP  ldPr) (tCall|+cCall|+gCall|+pCall|+pr3Call)  (prs $ fourK tN cN gN pN)
  assertState (four' find' c    ldC    ldG ldP ldPr ldTr) (cCall|+gCall|+pCall|+pr3Call|+trCall) [(fourK cN gN pN trRN,tr)]
  
  assertState (four' findUnambiguous' root ldRoot ldR ldT ldC  ldG)  (rootCall|+rCall|+tCall|+cCall|+gCall) (Just g) 
  assertState (four' findUnambiguous' r    ldR    ldT ldC ldG  ldP)  (rCall|+tCall|+cCall|+gCall|+pCall)    (Just p)
  assertState (four' findUnambiguous' c    ldC    ldG ldP ldPr ldTr) (cCall|+gCall|+pCall|+pr3Call|+trCall) (Just tr)
  
  assertState (four rN tN cN   gN   find' root ldRoot ldR ldT ldC  ldG)  (rootCall|+rCall|+tCall|+cCall|+gCall) [(fourK rootN rN tN cN,g)]
  assertState (four tN cN gN   pN   find' r    ldR    ldT ldC ldG  ldP)  (rCall|+tCall|+cCall|+gCall|+pCall)    [(fourK rN tN cN gN,p)]
  assertState (four cN gN pN   sRN  find' t    ldT    ldC ldG ldP  ldPr) (tCall|+cCall|+gCall|+pCall|+prCall)   [(fourK tN cN gN pN,pSr)]
  assertState (four cN gN pN   tfRN find' t    ldT    ldC ldG ldP  ldPr) (tCall|+cCall|+gCall|+pCall|+prCall)   [(fourK tN cN gN pN,pTfr)]
  assertState (four cN gN pN   trRN find' t    ldT    ldC ldG ldP  ldPr) (tCall|+cCall|+gCall|+pCall|+prCall)   [(fourK tN cN gN pN,pTrr)]
  assertState (four gN pN trRN trN  find' c    ldC    ldG ldP ldPr ldTr) (cCall|+gCall|+pCall|+prCall|+trCall)  [(fourK cN gN pN trRN,tr)]
                    
  assertState (four rN tN cN   gN   findUnambiguous' root ldRoot ldR ldT ldC  ldG)  (rootCall|+rCall|+tCall|+cCall|+gCall) (Just g) 
  assertState (four tN cN gN   pN   findUnambiguous' r    ldR    ldT ldC ldG  ldP)  (rCall|+tCall|+cCall|+gCall|+pCall)    (Just p)
  assertState (four cN gN pN   sRN  findUnambiguous' t    ldT    ldC ldG ldP  ldPr) (tCall|+cCall|+gCall|+pCall|+prCall)   (Just pSr)
  assertState (four cN gN pN   tfRN findUnambiguous' t    ldT    ldC ldG ldP  ldPr) (tCall|+cCall|+gCall|+pCall|+prCall)   (Just pTfr)
  assertState (four cN gN pN   trRN findUnambiguous' t    ldT    ldC ldG ldP  ldPr) (tCall|+cCall|+gCall|+pCall|+prCall)   (Just pTrr)
  assertState (four gN pN trRN trN  findUnambiguous' c    ldC    ldG ldP ldPr ldTr) (cCall|+gCall|+pCall|+prCall|+trCall)  (Just tr)
  
test_five = do
  assertState (five' find' root ldRoot ldR ldT ldC ldG  ldP)  (rootCall|+rCall|+tCall|+cCall|+gCall|+pCall) [(fiveK rootN rN tN cN gN,p)]
  assertState (five' find' r    ldR    ldT ldC ldG ldP  ldPr) (rCall|+tCall|+cCall|+gCall|+pCall|+pr3Call)  (prs $ fiveK rN tN cN gN pN)
  assertState (five' find' t    ldT    ldC ldG ldP ldPr ldTr) (tCall|+cCall|+gCall|+pCall|+pr3Call|+trCall) [(fiveK tN cN gN pN trRN,tr)]
  
  assertState (five' findUnambiguous' root ldRoot ldR ldT ldC ldG  ldP)  (rootCall|+rCall|+tCall|+cCall|+gCall|+pCall) (Just p) 
  assertState (five' findUnambiguous' t    ldT    ldC ldG ldP ldPr ldTr) (tCall|+cCall|+gCall|+pCall|+pr3Call|+trCall) (Just tr)
  
  assertState (five rN tN cN gN   pN   find' root ldRoot ldR ldT ldC ldG  ldP)  (rootCall|+rCall|+tCall|+cCall|+gCall|+pCall) [(fiveK rootN rN tN cN gN,p)]
  assertState (five tN cN gN pN   sRN  find' r    ldR    ldT ldC ldG ldP  ldPr) (rCall|+tCall|+cCall|+gCall|+pCall|+prCall)   [(fiveK rN tN cN gN pN,pSr)]
  assertState (five tN cN gN pN   tfRN find' r    ldR    ldT ldC ldG ldP  ldPr) (rCall|+tCall|+cCall|+gCall|+pCall|+prCall)   [(fiveK rN tN cN gN pN,pTfr)]
  assertState (five tN cN gN pN   trRN find' r    ldR    ldT ldC ldG ldP  ldPr) (rCall|+tCall|+cCall|+gCall|+pCall|+prCall)   [(fiveK rN tN cN gN pN,pTrr)]
  assertState (five cN gN pN trRN trN  find' t    ldT    ldC ldG ldP ldPr ldTr) (tCall|+cCall|+gCall|+pCall|+prCall|+trCall)  [(fiveK tN cN gN pN trRN,tr)]
  
  assertState (five rN tN cN gN   pN   findUnambiguous' root ldRoot ldR ldT ldC ldG  ldP)  (rootCall|+rCall|+tCall|+cCall|+gCall|+pCall) (Just p) 
  assertState (five tN cN gN pN   sRN  findUnambiguous' r    ldR    ldT ldC ldG ldP  ldPr) (rCall|+tCall|+cCall|+gCall|+pCall|+prCall)   (Just pSr)
  assertState (five tN cN gN pN   tfRN findUnambiguous' r    ldR    ldT ldC ldG ldP  ldPr) (rCall|+tCall|+cCall|+gCall|+pCall|+prCall)   (Just pTfr)
  assertState (five tN cN gN pN   trRN findUnambiguous' r    ldR    ldT ldC ldG ldP  ldPr) (rCall|+tCall|+cCall|+gCall|+pCall|+prCall)   (Just pTrr)
  assertState (five cN gN pN trRN trN  findUnambiguous' t    ldT    ldC ldG ldP ldPr ldTr) (tCall|+cCall|+gCall|+pCall|+prCall|+trCall)  (Just tr)
  
test_six = do
  assertState (six' find' root ldRoot ldR ldT ldC ldG ldP  ldPr) (rootCall|+rCall|+tCall|+cCall|+gCall|+pCall|+pr3Call) (prs $ sixK rootN rN tN cN gN pN)
  assertState (six' find' r    ldR    ldT ldC ldG ldP ldPr ldTr) (rCall|+tCall|+cCall|+gCall|+pCall|+pr3Call|+trCall)   [(sixK rN tN cN gN pN trRN,tr)]
  
  assertState (six' findUnambiguous' r    ldR    ldT ldC ldG ldP ldPr ldTr) (rCall|+tCall|+cCall|+gCall|+pCall|+pr3Call|+trCall) (Just tr)
  
  assertState (six rN tN cN gN pN   sRN  find' root ldRoot ldR ldT ldC ldG ldP  ldPr) (rootCall|+rCall|+tCall|+cCall|+gCall|+pCall|+prCall) [(sixK rootN rN tN cN gN pN,pSr)]
  assertState (six rN tN cN gN pN   tfRN find' root ldRoot ldR ldT ldC ldG ldP  ldPr) (rootCall|+rCall|+tCall|+cCall|+gCall|+pCall|+prCall) [(sixK rootN rN tN cN gN pN,pTfr)]
  assertState (six rN tN cN gN pN   trRN find' root ldRoot ldR ldT ldC ldG ldP  ldPr) (rootCall|+rCall|+tCall|+cCall|+gCall|+pCall|+prCall) [(sixK rootN rN tN cN gN pN,pTrr)]
  assertState (six tN cN gN pN trRN trN  find' r    ldR    ldT ldC ldG ldP ldPr ldTr) (rCall|+tCall|+cCall|+gCall|+pCall|+prCall|+trCall)   [(sixK rN tN cN gN pN trRN,tr)]
  
  assertState (six rN tN cN gN pN   sRN  findUnambiguous' root ldRoot ldR ldT ldC ldG ldP  ldPr) (rootCall|+rCall|+tCall|+cCall|+gCall|+pCall|+prCall) (Just pSr)
  assertState (six rN tN cN gN pN   tfRN findUnambiguous' root ldRoot ldR ldT ldC ldG ldP  ldPr) (rootCall|+rCall|+tCall|+cCall|+gCall|+pCall|+prCall) (Just pTfr)
  assertState (six rN tN cN gN pN   trRN findUnambiguous' root ldRoot ldR ldT ldC ldG ldP  ldPr) (rootCall|+rCall|+tCall|+cCall|+gCall|+pCall|+prCall) (Just pTrr)
  assertState (six tN cN gN pN trRN trN  findUnambiguous' r    ldR    ldT ldC ldG ldP ldPr ldTr) (rCall|+tCall|+cCall|+gCall|+pCall|+prCall|+trCall)   (Just tr)
  
test_seven = do
  assertState (seven' find'            root ldRoot ldR ldT ldC ldG ldP ldPr ldTr) (rootCall|+rCall|+tCall|+cCall|+gCall|+pCall|+pr3Call|+trCall) [(sevenK rootN rN tN cN gN pN trRN,tr)]
  assertState (seven' findUnambiguous' root ldRoot ldR ldT ldC ldG ldP ldPr ldTr) (rootCall|+rCall|+tCall|+cCall|+gCall|+pCall|+pr3Call|+trCall) (Just tr)
  
  assertState (seven rN tN cN gN pN trRN trN find'            root ldRoot ldR ldT ldC ldG ldP ldPr ldTr) (rootCall|+rCall|+tCall|+cCall|+gCall|+pCall|+prCall|+trCall) [(sevenK rootN rN tN cN gN pN trRN,tr)]
  assertState (seven rN tN cN gN pN trRN trN findUnambiguous' root ldRoot ldR ldT ldC ldG ldP ldPr ldTr) (rootCall|+rCall|+tCall|+cCall|+gCall|+pCall|+prCall|+trCall) (Just tr)
  
  
  