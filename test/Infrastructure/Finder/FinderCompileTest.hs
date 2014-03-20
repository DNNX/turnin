{-# LANGUAGE  FlexibleInstances, UndecidableInstances #-}
{-# OPTIONS_GHC -F -pgmF htfpp -fno-warn-incomplete-patterns #-}
module Infrastructure.Finder.FinderCompileTest where

import Test.Framework
import Infrastructure.Finder.FinderTestUtils

{-# ANN module "HLint: ignore Use camelCase" #-}
{-# ANN module "HLint: ignore Reduce duplication" #-}
{-# ANN module "HLint: ignore Evaluate" #-}
{-# ANN module "HLint: ignore Use ++" #-}

test_zero = do
  assertState (zero find' root ldRoot) rootCall [(zeroK, root)]
  assertState (zero find' r    ldR)    rCall    [(zeroK, r)]
  assertState (zero find' t    ldT)    tCall    [(zeroK, t)]
  assertState (zero find' c    ldC)    cCall    [(zeroK, c)]
  assertState (zero find' g    ldG)    gCall    [(zeroK, g)]
  assertState (zero find' p    ldP)    pCall    [(zeroK, p)]
  assertState (zero find' pSr  ldPr)   prCall   [(zeroK, pSr)]
  assertState (zero find' pTfr ldPr)   prCall   [(zeroK, pTfr)]
  assertState (zero find' pTrr ldPr)   prCall   [(zeroK, pTrr)]
  assertState (zero find' tr   ldTr)   trCall   [(zeroK, tr)]
  
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

test_one = let prk = oneK pN in do
  assertState (one' find' root ldRoot ldR)  (rootCall|+rCall) [(oneK rootN,r)]
  assertState (one' find' r    ldR    ldT)  (rCall|+tCall)    [(oneK rN  ,t)]
  assertState (one' find' t    ldT    ldC)  (tCall|+cCall)    [(oneK tN  ,c)]
  assertState (one' find' c    ldC    ldG)  (cCall|+gCall)    [(oneK cN  ,g)]
  assertState (one' find' g    ldG    ldP)  (gCall|+pCall)    [(oneK gN  ,p)]
  assertState (one' find' p    ldP    ldPr) (pCall|+pr3Call)  [(prk ,pSr),(prk, pTfr),(prk, pTrr)]
  assertState (one' find' pTrr ldPr   ldTr) (prCall|+trCall)  [(oneK trRN ,tr)]
  
  assertState (one' findUnambiguous' root ldRoot ldR)  (rootCall|+rCall) (Just r)  
  assertState (one' findUnambiguous' r    ldR    ldT)  (rCall|+tCall)    (Just t)  
  assertState (one' findUnambiguous' t    ldT    ldC)  (tCall|+cCall)    (Just c)  
  assertState (one' findUnambiguous' c    ldC    ldG)  (cCall|+gCall)    (Just g)  
  assertState (one' findUnambiguous' g    ldG    ldP)  (gCall|+pCall)    (Just p)  
  assertState (one' findUnambiguous' pTrr ldPr   ldTr) (prCall|+trCall)  (Just tr) 

test_two = let prk = twoK gN pN in do
  assertState (two' find' root ldRoot ldR  ldT)  (rootCall|+rCall|+tCall) [(twoK rootN rN ,t)]
  assertState (two' find' r    ldR    ldT  ldC)  (rCall|+tCall|+cCall)    [(twoK rN tN ,c)]
  assertState (two' find' t    ldT    ldC  ldG)  (tCall|+cCall|+gCall)    [(twoK tN cN ,g)]
  assertState (two' find' c    ldC    ldG  ldP)  (cCall|+gCall|+pCall)    [(twoK cN gN ,p)]
  assertState (two' find' g    ldG    ldP  ldPr) (gCall|+pCall|+pr3Call)  [(prk ,pSr),(prk, pTfr),(prk, pTrr)]
  assertState (two' find' p    ldP    ldPr ldTr) (pCall|+pr3Call|+trCall) [(twoK pN trRN,tr)]
  
  assertState (two' findUnambiguous' root ldRoot ldR  ldT)  (rootCall|+rCall|+tCall) (Just t) 
  assertState (two' findUnambiguous' r    ldR    ldT  ldC)  (rCall|+tCall|+cCall)    (Just c) 
  assertState (two' findUnambiguous' t    ldT    ldC  ldG)  (tCall|+cCall|+gCall)    (Just g) 
  assertState (two' findUnambiguous' c    ldC    ldG  ldP)  (cCall|+gCall|+pCall)    (Just p) 
  assertState (two' findUnambiguous' p    ldP    ldPr ldTr) (pCall|+pr3Call|+trCall) (Just tr)

test_three = let prk = threeK cN gN pN in do
  assertState (three' find' root ldRoot ldR ldT  ldC)  (rootCall|+rCall|+tCall|+cCall) [(threeK rootN rN tN,c)]
  assertState (three' find' r    ldR    ldT ldC  ldG)  (rCall|+tCall|+cCall|+gCall)    [(threeK rN tN cN,g)]
  assertState (three' find' t    ldT    ldC ldG  ldP)  (tCall|+cCall|+gCall|+pCall)    [(threeK tN cN gN,p)]
  assertState (three' find' c    ldC    ldG ldP  ldPr) (cCall|+gCall|+pCall|+pr3Call)  [(prk ,pSr),(prk, pTfr),(prk, pTrr)]
  assertState (three' find' g    ldG    ldP ldPr ldTr) (gCall|+pCall|+pr3Call|+trCall) [(threeK gN pN trRN,tr)]
  
  assertState (three' findUnambiguous' root ldRoot ldR ldT  ldC)  (rootCall|+rCall|+tCall|+cCall) (Just c)                        
  assertState (three' findUnambiguous' r    ldR    ldT ldC  ldG)  (rCall|+tCall|+cCall|+gCall)    (Just g)                        
  assertState (three' findUnambiguous' t    ldT    ldC ldG  ldP)  (tCall|+cCall|+gCall|+pCall)    (Just p)                        
  assertState (three' findUnambiguous' g    ldG    ldP ldPr ldTr) (gCall|+pCall|+pr3Call|+trCall) (Just tr)                       

test_four = let prk = fourK tN cN gN pN in do
  assertState (four' find' root ldRoot ldR ldT ldC  ldG)  (rootCall|+rCall|+tCall|+cCall|+gCall) [(fourK rootN rN tN cN,g)]
  assertState (four' find' r    ldR    ldT ldC ldG  ldP)  (rCall|+tCall|+cCall|+gCall|+pCall)    [(fourK rN tN cN gN,p)]
  assertState (four' find' t    ldT    ldC ldG ldP  ldPr) (tCall|+cCall|+gCall|+pCall|+pr3Call)  [(prk ,pSr),(prk, pTfr),(prk, pTrr)]
  assertState (four' find' c    ldC    ldG ldP ldPr ldTr) (cCall|+gCall|+pCall|+pr3Call|+trCall) [(fourK cN gN pN trRN,tr)]
  
  assertState (four' findUnambiguous' root ldRoot ldR ldT ldC  ldG)  (rootCall|+rCall|+tCall|+cCall|+gCall) (Just g) 
  assertState (four' findUnambiguous' r    ldR    ldT ldC ldG  ldP)  (rCall|+tCall|+cCall|+gCall|+pCall)    (Just p) 
  assertState (four' findUnambiguous' c    ldC    ldG ldP ldPr ldTr) (cCall|+gCall|+pCall|+pr3Call|+trCall) (Just tr)

test_five = let prk = fiveK rN tN cN gN pN in do
  assertState (five' find' root ldRoot ldR ldT ldC ldG  ldP)  (rootCall|+rCall|+tCall|+cCall|+gCall|+pCall) [(fiveK rootN rN tN cN gN,p)]
  assertState (five' find' r    ldR    ldT ldC ldG ldP  ldPr) (rCall|+tCall|+cCall|+gCall|+pCall|+pr3Call)  [(prk ,pSr),(prk, pTfr),(prk, pTrr)]
  assertState (five' find' t    ldT    ldC ldG ldP ldPr ldTr) (tCall|+cCall|+gCall|+pCall|+pr3Call|+trCall) [(fiveK tN cN gN pN trRN,tr)]
  
  assertState (five' findUnambiguous' root ldRoot ldR ldT ldC ldG  ldP)  (rootCall|+rCall|+tCall|+cCall|+gCall|+pCall) (Just p) 
  assertState (five' findUnambiguous' t    ldT    ldC ldG ldP ldPr ldTr) (tCall|+cCall|+gCall|+pCall|+pr3Call|+trCall) (Just tr)

test_six = let prk = sixK rootN rN tN cN gN pN in do
  assertState (six' find' root ldRoot ldR ldT ldC ldG ldP  ldPr) (rootCall|+rCall|+tCall|+cCall|+gCall|+pCall|+pr3Call) [(prk ,pSr),(prk, pTfr),(prk, pTrr)]
  assertState (six' find' r    ldR    ldT ldC ldG ldP ldPr ldTr) (rCall|+tCall|+cCall|+gCall|+pCall|+pr3Call|+trCall)   [(sixK rN tN cN gN pN trRN,tr)]
  
  assertState (six' findUnambiguous' r    ldR    ldT ldC ldG ldP ldPr ldTr) (rCall|+tCall|+cCall|+gCall|+pCall|+pr3Call|+trCall) (Just tr)

test_sevenRoot = do
  assertState (seven' find' root ldRoot ldR ldT ldC ldG ldP ldPr ldTr) (rootCall|+rCall|+tCall|+cCall|+gCall|+pCall|+pr3Call|+trCall)   [(sevenK rootN rN tN cN gN pN trRN,tr)]
  assertState (seven' findUnambiguous' root ldRoot ldR ldT ldC ldG ldP ldPr ldTr) (rootCall|+rCall|+tCall|+cCall|+gCall|+pCall|+pr3Call|+trCall) (Just tr)
