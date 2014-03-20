{-# OPTIONS_GHC -F -pgmF htfpp -fno-warn-incomplete-patterns#-}
module Infrastructure.Finder.FinderPropFindingTest where

import Test.Framework
import Infrastructure.Finder.FinderTestUtils

import Infrastructure.Node
import Domain.Root
import Domain.Project
import Domain.ProjectRepo
import Domain.SubmitRepo
import Domain.TrainFileRepo

import System.IO.Unsafe

{-# ANN module "HLint: ignore Use camelCase" #-}
{-# ANN module "HLint: ignore Reduce duplication" #-}
{-# ANN module "HLint: ignore Evaluate" #-}
{-# ANN module "HLint: ignore Use ++" #-} 

sRN = getName emptySubmitRepo
tfRN = getName emptyTrainFileRepo
trRN = getName emptyTrainRunRepo

prop_zero rootN rN tN cN gN pN trN sKey sContent tfKey tfContent = 
 "" `notElem` [rootN,rN,tN,cN,gN,pN,trN,sKey,sContent,tfKey,tfContent] ==>  
  let tr = make trN
      p = setSubmitRepo (setTrainFileRepo (setTrainRunRepo (make pN) trr) tfr) sr
      g = addChild (make gN) p
      c = addChild (make cN) g
      t = addChild (make tN) c
      r = addChild (make rN) t
      root = addChild (make rootN :: Root) r
      
      sr = addSubmit emptySubmitRepo sKey sContent
      tfr = addTrainFile emptyTrainFileRepo tfKey tfContent
      trr = addChild emptyTrainRunRepo tr
      
      pSr = makeProjectSubmitRepo sr
      pTfr = makeProjectTrainFileRepo tfr
      pTrr = makeProjectTrainRunRepo trr
  
  in assertStateP (zero find' root ldRoot) rootCall [(zeroK,root)]
  && assertStateP (zero find' r    ldR)    rCall    [(zeroK,r)]
  && assertStateP (zero find' t    ldT)    tCall    [(zeroK,t)]
  && assertStateP (zero find' c    ldC)    cCall    [(zeroK,c)]
  && assertStateP (zero find' g    ldG)    gCall    [(zeroK,g)]
  && assertStateP (zero find' p    ldP)    pCall    [(zeroK,p)]
  && assertStateP (zero find' pSr  ldPr)   prCall   [(zeroK,pSr)]
  && assertStateP (zero find' pTfr ldPr)   prCall   [(zeroK,pTfr)]
  && assertStateP (zero find' pTrr ldPr)   prCall   [(zeroK,pTrr)]
  && assertStateP (zero find' tr   ldTr)   trCall   [(zeroK,tr)]
            
  && assertStateP (zero findUnambiguous' root ldRoot) rootCall (Just root)
  && assertStateP (zero findUnambiguous' r    ldR)    rCall    (Just r)
  && assertStateP (zero findUnambiguous' t    ldT)    tCall    (Just t)
  && assertStateP (zero findUnambiguous' c    ldC)    cCall    (Just c)
  && assertStateP (zero findUnambiguous' g    ldG)    gCall    (Just g)
  && assertStateP (zero findUnambiguous' p    ldP)    pCall    (Just p)
  && assertStateP (zero findUnambiguous' pSr  ldPr)   prCall   (Just pSr)
  && assertStateP (zero findUnambiguous' pTfr ldPr)   prCall   (Just pTfr)
  && assertStateP (zero findUnambiguous' pTrr ldPr)   prCall   (Just pTrr)
  && assertStateP (zero findUnambiguous' tr   ldTr)   trCall   (Just tr)

prop_one rootN rN tN cN gN pN trN sKey sContent tfKey tfContent = 
 "" `notElem` [rootN,rN,tN,cN,gN,pN,trN,sKey,sContent,tfKey,tfContent] ==>  
  let tr = make trN
      p = setSubmitRepo (setTrainFileRepo (setSubmitRepo (setTrainFileRepo (setTrainRunRepo (make pN) trr) tfr) sr) tfr) sr
      g = addChild (make gN) p
      c = addChild (make cN) g
      t = addChild (make tN) c
      r = addChild (make rN) t
      root = addChild (make rootN :: Root) r
      
      sr = addSubmit emptySubmitRepo sKey sContent
      tfr = addTrainFile emptyTrainFileRepo tfKey tfContent
      trr = addChild emptyTrainRunRepo tr
      
      pSr = makeProjectSubmitRepo sr
      pTfr = makeProjectTrainFileRepo tfr
      pTrr = makeProjectTrainRunRepo trr
      
      prs k = [(k,pSr),(k,pTfr),(k,pTrr)]      
  
  in assertStateP (one' find' root ldRoot ldR)  (rootCall|+rCall) [(oneK rootN,r)]
  && assertStateP (one' find' r    ldR    ldT)  (rCall|+tCall)    [(oneK rN,t)]
  && assertStateP (one' find' t    ldT    ldC)  (tCall|+cCall)    [(oneK tN,c)]
  && assertStateP (one' find' c    ldC    ldG)  (cCall|+gCall)    [(oneK cN,g)]
  && assertStateP (one' find' g    ldG    ldP)  (gCall|+pCall)    [(oneK gN,p)]
  && assertStateP (one' find' p    ldP    ldPr) (pCall|+pr3Call)  (prs $ oneK pN)
  && assertStateP (one' find' pTrr ldPr   ldTr) (prCall|+trCall)  [(oneK trRN,tr)]
  
  && assertStateP (one' findUnambiguous' root ldRoot ldR)  (rootCall|+rCall) (Just r)
  && assertStateP (one' findUnambiguous' r    ldR    ldT)  (rCall|+tCall)    (Just t)
  && assertStateP (one' findUnambiguous' t    ldT    ldC)  (tCall|+cCall)    (Just c)
  && assertStateP (one' findUnambiguous' c    ldC    ldG)  (cCall|+gCall)    (Just g)
  && assertStateP (one' findUnambiguous' g    ldG    ldP)  (gCall|+pCall)    (Just p)
  && assertStateP (one' findUnambiguous' pTrr ldPr   ldTr) (prCall|+trCall)  (Just tr)
  
  && assertStateP (one rN   find' root ldRoot ldR)  (rootCall|+rCall) [(oneK rootN,r)]
  && assertStateP (one tN   find' r    ldR    ldT)  (rCall|+tCall)    [(oneK rN,t)]
  && assertStateP (one cN   find' t    ldT    ldC)  (tCall|+cCall)    [(oneK tN,c)]
  && assertStateP (one gN   find' c    ldC    ldG)  (cCall|+gCall)    [(oneK cN,g)]
  && assertStateP (one pN   find' g    ldG    ldP)  (gCall|+pCall)    [(oneK gN,p)]
  && assertStateP (one sRN  find' p    ldP    ldPr) (pCall|+prCall)   [(oneK pN,pSr)]
  && assertStateP (one tfRN find' p    ldP    ldPr) (pCall|+prCall)   [(oneK pN,pTfr)]
  && assertStateP (one trRN find' p    ldP    ldPr) (pCall|+prCall)   [(oneK pN,pTrr)]
  && assertStateP (one trN  find' pTrr ldPr   ldTr) (prCall|+trCall)  [(oneK trRN,tr)]
  
  && assertStateP (one rN   findUnambiguous' root ldRoot ldR)  (rootCall|+rCall) (Just r)
  && assertStateP (one tN   findUnambiguous' r    ldR    ldT)  (rCall|+tCall)    (Just t)
  && assertStateP (one cN   findUnambiguous' t    ldT    ldC)  (tCall|+cCall)    (Just c)
  && assertStateP (one gN   findUnambiguous' c    ldC    ldG)  (cCall|+gCall)    (Just g)
  && assertStateP (one pN   findUnambiguous' g    ldG    ldP)  (gCall|+pCall)    (Just p)
  && assertStateP (one sRN  findUnambiguous' p    ldP    ldPr) (pCall|+prCall)   (Just pSr)
  && assertStateP (one tfRN findUnambiguous' p    ldP    ldPr) (pCall|+prCall)   (Just pTfr)
  && assertStateP (one trRN findUnambiguous' p    ldP    ldPr) (pCall|+prCall)   (Just pTrr)
  && assertStateP (one trN  findUnambiguous' pTrr ldPr   ldTr) (prCall|+trCall)  (Just tr)

prop_two rootN rN tN cN gN pN trN sKey sContent tfKey tfContent = 
 "" `notElem` [rootN,rN,tN,cN,gN,pN,trN,sKey,sContent,tfKey,tfContent] ==>  
  let tr = make trN
      p = setSubmitRepo (setTrainFileRepo (setTrainRunRepo (make pN) trr) tfr) sr
      g = addChild (make gN) p
      c = addChild (make cN) g
      t = addChild (make tN) c
      r = addChild (make rN) t
      root = addChild (make rootN :: Root) r
      
      sr = addSubmit emptySubmitRepo sKey sContent
      tfr = addTrainFile emptyTrainFileRepo tfKey tfContent
      trr = addChild emptyTrainRunRepo tr
      
      pSr = makeProjectSubmitRepo sr
      pTfr = makeProjectTrainFileRepo tfr
      pTrr = makeProjectTrainRunRepo trr
      
      prs k = [(k,pSr),(k,pTfr),(k,pTrr)]
  
  in assertStateP (two' find' root ldRoot ldR  ldT)  (rootCall|+rCall|+tCall) [(twoK rootN rN,t)]
  && assertStateP (two' find' r    ldR    ldT  ldC)  (rCall|+tCall|+cCall)    [(twoK rN tN,c)]
  && assertStateP (two' find' t    ldT    ldC  ldG)  (tCall|+cCall|+gCall)    [(twoK tN cN,g)]
  && assertStateP (two' find' c    ldC    ldG  ldP)  (cCall|+gCall|+pCall)    [(twoK cN gN,p)]
  && assertStateP (two' find' g    ldG    ldP  ldPr) (gCall|+pCall|+pr3Call)  (prs $ twoK gN pN)
  && assertStateP (two' find' p    ldP    ldPr ldTr) (pCall|+pr3Call|+trCall) [(twoK pN trRN,tr)]

  && assertStateP (two' findUnambiguous' root ldRoot ldR  ldT)  (rootCall|+rCall|+tCall) (Just t)
  && assertStateP (two' findUnambiguous' r    ldR    ldT  ldC)  (rCall|+tCall|+cCall)    (Just c)
  && assertStateP (two' findUnambiguous' t    ldT    ldC  ldG)  (tCall|+cCall|+gCall)    (Just g)
  && assertStateP (two' findUnambiguous' c    ldC    ldG  ldP)  (cCall|+gCall|+pCall)    (Just p)
  && assertStateP (two' findUnambiguous' p    ldP    ldPr ldTr) (pCall|+pr3Call|+trCall) (Just tr)

  && assertStateP (two rN   tN   find' root ldRoot ldR  ldT)  (rootCall|+rCall|+tCall) [(twoK rootN rN,t)]
  && assertStateP (two tN   cN   find' r    ldR    ldT  ldC)  (rCall|+tCall|+cCall)    [(twoK rN tN,c)]
  && assertStateP (two cN   gN   find' t    ldT    ldC  ldG)  (tCall|+cCall|+gCall)    [(twoK tN cN,g)]
  && assertStateP (two gN   pN   find' c    ldC    ldG  ldP)  (cCall|+gCall|+pCall)    [(twoK cN gN,p)]
  && assertStateP (two pN   sRN  find' g    ldG    ldP  ldPr) (gCall|+pCall|+prCall)   [(twoK gN pN,pSr)]
  && assertStateP (two pN   tfRN find' g    ldG    ldP  ldPr) (gCall|+pCall|+prCall)   [(twoK gN pN,pTfr)]
  && assertStateP (two pN   trRN find' g    ldG    ldP  ldPr) (gCall|+pCall|+prCall)   [(twoK gN pN,pTrr)]
  && assertStateP (two trRN trN  find' p    ldP    ldPr ldTr) (pCall|+prCall|+trCall)  [(twoK pN trRN,tr)]

  && assertStateP (two rN   tN   findUnambiguous' root ldRoot ldR  ldT)  (rootCall|+rCall|+tCall) (Just t)
  && assertStateP (two tN   cN   findUnambiguous' r    ldR    ldT  ldC)  (rCall|+tCall|+cCall)    (Just c)
  && assertStateP (two cN   gN   findUnambiguous' t    ldT    ldC  ldG)  (tCall|+cCall|+gCall)    (Just g)
  && assertStateP (two gN   pN   findUnambiguous' c    ldC    ldG  ldP)  (cCall|+gCall|+pCall)    (Just p)
  && assertStateP (two pN   sRN  findUnambiguous' g    ldG    ldP  ldPr) (gCall|+pCall|+prCall)   (Just pSr)
  && assertStateP (two pN   tfRN findUnambiguous' g    ldG    ldP  ldPr) (gCall|+pCall|+prCall)   (Just pTfr)
  && assertStateP (two pN   trRN findUnambiguous' g    ldG    ldP  ldPr) (gCall|+pCall|+prCall)   (Just pTrr)
  && assertStateP (two trRN trN  findUnambiguous' p    ldP    ldPr ldTr) (pCall|+prCall|+trCall)  (Just tr)

prop_three rootN rN tN cN gN pN trN sKey sContent tfKey tfContent = 
 "" `notElem` [rootN,rN,tN,cN,gN,pN,trN,sKey,sContent,tfKey,tfContent] ==>  
  let tr = make trN
      p = setSubmitRepo (setTrainFileRepo (setTrainRunRepo (make pN) trr) tfr) sr
      g = addChild (make gN) p
      c = addChild (make cN) g
      t = addChild (make tN) c
      r = addChild (make rN) t
      root = addChild (make rootN :: Root) r
      
      sr = addSubmit emptySubmitRepo sKey sContent
      tfr = addTrainFile emptyTrainFileRepo tfKey tfContent
      trr = addChild emptyTrainRunRepo tr
      
      pSr = makeProjectSubmitRepo sr
      pTfr = makeProjectTrainFileRepo tfr
      pTrr = makeProjectTrainRunRepo trr
      
      prs k = [(k,pSr),(k,pTfr),(k,pTrr)]
  
  in assertStateP (three' find' root ldRoot ldR ldT  ldC)  (rootCall|+rCall|+tCall|+cCall) [(threeK rootN rN tN,c)]
  && assertStateP (three' find' r    ldR    ldT ldC  ldG)  (rCall|+tCall|+cCall|+gCall)    [(threeK rN tN cN,g)]
  && assertStateP (three' find' t    ldT    ldC ldG  ldP)  (tCall|+cCall|+gCall|+pCall)    [(threeK tN cN gN,p)]
  && assertStateP (three' find' c    ldC    ldG ldP  ldPr) (cCall|+gCall|+pCall|+pr3Call)  (prs $ threeK cN gN pN)
  && assertStateP (three' find' g    ldG    ldP ldPr ldTr) (gCall|+pCall|+pr3Call|+trCall) [(threeK gN pN trRN,tr)]

  && assertStateP (three' findUnambiguous' root ldRoot ldR ldT  ldC)  (rootCall|+rCall|+tCall|+cCall) (Just c)
  && assertStateP (three' findUnambiguous' r    ldR    ldT ldC  ldG)  (rCall|+tCall|+cCall|+gCall)    (Just g)
  && assertStateP (three' findUnambiguous' t    ldT    ldC ldG  ldP)  (tCall|+cCall|+gCall|+pCall)    (Just p)
  && assertStateP (three' findUnambiguous' g    ldG    ldP ldPr ldTr) (gCall|+pCall|+pr3Call|+trCall) (Just tr)
  
  && assertStateP (three rN tN   cN   find' root ldRoot ldR ldT  ldC)  (rootCall|+rCall|+tCall|+cCall) [(threeK rootN rN tN,c)]
  && assertStateP (three tN cN   gN   find' r    ldR    ldT ldC  ldG)  (rCall|+tCall|+cCall|+gCall)    [(threeK rN tN cN,g)]
  && assertStateP (three cN gN   pN   find' t    ldT    ldC ldG  ldP)  (tCall|+cCall|+gCall|+pCall)    [(threeK tN cN gN,p)]
  && assertStateP (three gN pN   sRN  find' c    ldC    ldG ldP  ldPr) (cCall|+gCall|+pCall|+prCall)   [(threeK cN gN pN,pSr)]
  && assertStateP (three gN pN   tfRN find' c    ldC    ldG ldP  ldPr) (cCall|+gCall|+pCall|+prCall)   [(threeK cN gN pN,pTfr)]
  && assertStateP (three gN pN   trRN find' c    ldC    ldG ldP  ldPr) (cCall|+gCall|+pCall|+prCall)   [(threeK cN gN pN,pTrr)]
  && assertStateP (three pN trRN trN  find' g    ldG    ldP ldPr ldTr) (gCall|+pCall|+prCall|+trCall)  [(threeK gN pN trRN,tr)]
                     
  && assertStateP (three rN tN   cN   findUnambiguous' root ldRoot ldR ldT  ldC)  (rootCall|+rCall|+tCall|+cCall) (Just c)
  && assertStateP (three tN cN   gN   findUnambiguous' r    ldR    ldT ldC  ldG)  (rCall|+tCall|+cCall|+gCall)    (Just g)
  && assertStateP (three cN gN   pN   findUnambiguous' t    ldT    ldC ldG  ldP)  (tCall|+cCall|+gCall|+pCall)    (Just p)
  && assertStateP (three gN pN   sRN  findUnambiguous' c    ldC    ldG ldP  ldPr) (cCall|+gCall|+pCall|+prCall)   (Just pSr)
  && assertStateP (three gN pN   tfRN findUnambiguous' c    ldC    ldG ldP  ldPr) (cCall|+gCall|+pCall|+prCall)   (Just pTfr)
  && assertStateP (three gN pN   trRN findUnambiguous' c    ldC    ldG ldP  ldPr) (cCall|+gCall|+pCall|+prCall)   (Just pTrr)
  && assertStateP (three pN trRN trN  findUnambiguous' g    ldG    ldP ldPr ldTr) (gCall|+pCall|+prCall|+trCall)  (Just tr)

prop_four rootN rN tN cN gN pN trN sKey sContent tfKey tfContent = 
 "" `notElem` [rootN,rN,tN,cN,gN,pN,trN,sKey,sContent,tfKey,tfContent] ==>  
  let tr = make trN
      p = setSubmitRepo (setTrainFileRepo (setTrainRunRepo (make pN) trr) tfr) sr
      g = addChild (make gN) p
      c = addChild (make cN) g
      t = addChild (make tN) c
      r = addChild (make rN) t
      root = addChild (make rootN :: Root) r
      
      sr = addSubmit emptySubmitRepo sKey sContent
      tfr = addTrainFile emptyTrainFileRepo tfKey tfContent
      trr = addChild emptyTrainRunRepo tr
      
      pSr = makeProjectSubmitRepo sr
      pTfr = makeProjectTrainFileRepo tfr
      pTrr = makeProjectTrainRunRepo trr
      
      prs k = [(k,pSr),(k,pTfr),(k,pTrr)]
  
  in assertStateP (four' find' root ldRoot ldR ldT ldC  ldG)  (rootCall|+rCall|+tCall|+cCall|+gCall) [(fourK rootN rN tN cN,g)]
  && assertStateP (four' find' r    ldR    ldT ldC ldG  ldP)  (rCall|+tCall|+cCall|+gCall|+pCall)    [(fourK rN tN cN gN,p)]
  && assertStateP (four' find' t    ldT    ldC ldG ldP  ldPr) (tCall|+cCall|+gCall|+pCall|+pr3Call)  (prs $ fourK tN cN gN pN)
  && assertStateP (four' find' c    ldC    ldG ldP ldPr ldTr) (cCall|+gCall|+pCall|+pr3Call|+trCall) [(fourK cN gN pN trRN,tr)]
  
  && assertStateP (four' findUnambiguous' root ldRoot ldR ldT ldC  ldG)  (rootCall|+rCall|+tCall|+cCall|+gCall) (Just g) 
  && assertStateP (four' findUnambiguous' r    ldR    ldT ldC ldG  ldP)  (rCall|+tCall|+cCall|+gCall|+pCall)    (Just p)
  && assertStateP (four' findUnambiguous' c    ldC    ldG ldP ldPr ldTr) (cCall|+gCall|+pCall|+pr3Call|+trCall) (Just tr)
  
  && assertStateP (four rN tN cN   gN   find' root ldRoot ldR ldT ldC  ldG)  (rootCall|+rCall|+tCall|+cCall|+gCall) [(fourK rootN rN tN cN,g)]
  && assertStateP (four tN cN gN   pN   find' r    ldR    ldT ldC ldG  ldP)  (rCall|+tCall|+cCall|+gCall|+pCall)    [(fourK rN tN cN gN,p)]
  && assertStateP (four cN gN pN   sRN  find' t    ldT    ldC ldG ldP  ldPr) (tCall|+cCall|+gCall|+pCall|+prCall)   [(fourK tN cN gN pN,pSr)]
  && assertStateP (four cN gN pN   tfRN find' t    ldT    ldC ldG ldP  ldPr) (tCall|+cCall|+gCall|+pCall|+prCall)   [(fourK tN cN gN pN,pTfr)]
  && assertStateP (four cN gN pN   trRN find' t    ldT    ldC ldG ldP  ldPr) (tCall|+cCall|+gCall|+pCall|+prCall)   [(fourK tN cN gN pN,pTrr)]
  && assertStateP (four gN pN trRN trN  find' c    ldC    ldG ldP ldPr ldTr) (cCall|+gCall|+pCall|+prCall|+trCall)  [(fourK cN gN pN trRN,tr)]
                    
  && assertStateP (four rN tN cN   gN   findUnambiguous' root ldRoot ldR ldT ldC  ldG)  (rootCall|+rCall|+tCall|+cCall|+gCall) (Just g) 
  && assertStateP (four tN cN gN   pN   findUnambiguous' r    ldR    ldT ldC ldG  ldP)  (rCall|+tCall|+cCall|+gCall|+pCall)    (Just p)
  && assertStateP (four cN gN pN   sRN  findUnambiguous' t    ldT    ldC ldG ldP  ldPr) (tCall|+cCall|+gCall|+pCall|+prCall)   (Just pSr)
  && assertStateP (four cN gN pN   tfRN findUnambiguous' t    ldT    ldC ldG ldP  ldPr) (tCall|+cCall|+gCall|+pCall|+prCall)   (Just pTfr)
  && assertStateP (four cN gN pN   trRN findUnambiguous' t    ldT    ldC ldG ldP  ldPr) (tCall|+cCall|+gCall|+pCall|+prCall)   (Just pTrr)
  && assertStateP (four gN pN trRN trN  findUnambiguous' c    ldC    ldG ldP ldPr ldTr) (cCall|+gCall|+pCall|+prCall|+trCall)  (Just tr)
  
prop_five rootN rN tN cN gN pN trN sKey sContent tfKey tfContent = 
 "" `notElem` [rootN,rN,tN,cN,gN,pN,trN,sKey,sContent,tfKey,tfContent] ==>  
  let tr = make trN
      p = setSubmitRepo (setTrainFileRepo (setTrainRunRepo (make pN) trr) tfr) sr
      g = addChild (make gN) p
      c = addChild (make cN) g
      t = addChild (make tN) c
      r = addChild (make rN) t
      root = addChild (make rootN :: Root) r
      
      sr = addSubmit emptySubmitRepo sKey sContent
      tfr = addTrainFile emptyTrainFileRepo tfKey tfContent
      trr = addChild emptyTrainRunRepo tr
      
      pSr = makeProjectSubmitRepo sr
      pTfr = makeProjectTrainFileRepo tfr
      pTrr = makeProjectTrainRunRepo trr
      
      prs k = [(k,pSr),(k,pTfr),(k,pTrr)]
  
  in assertStateP (five' find' root ldRoot ldR ldT ldC ldG  ldP)  (rootCall|+rCall|+tCall|+cCall|+gCall|+pCall) [(fiveK rootN rN tN cN gN,p)]
  && assertStateP (five' find' r    ldR    ldT ldC ldG ldP  ldPr) (rCall|+tCall|+cCall|+gCall|+pCall|+pr3Call)  (prs $ fiveK rN tN cN gN pN)
  && assertStateP (five' find' t    ldT    ldC ldG ldP ldPr ldTr) (tCall|+cCall|+gCall|+pCall|+pr3Call|+trCall) [(fiveK tN cN gN pN trRN,tr)]
  
  && assertStateP (five' findUnambiguous' root ldRoot ldR ldT ldC ldG  ldP)  (rootCall|+rCall|+tCall|+cCall|+gCall|+pCall) (Just p) 
  && assertStateP (five' findUnambiguous' t    ldT    ldC ldG ldP ldPr ldTr) (tCall|+cCall|+gCall|+pCall|+pr3Call|+trCall) (Just tr)
  
  && assertStateP (five rN tN cN gN   pN   find' root ldRoot ldR ldT ldC ldG  ldP)  (rootCall|+rCall|+tCall|+cCall|+gCall|+pCall) [(fiveK rootN rN tN cN gN,p)]
  && assertStateP (five tN cN gN pN   sRN  find' r    ldR    ldT ldC ldG ldP  ldPr) (rCall|+tCall|+cCall|+gCall|+pCall|+prCall)   [(fiveK rN tN cN gN pN,pSr)]
  && assertStateP (five tN cN gN pN   tfRN find' r    ldR    ldT ldC ldG ldP  ldPr) (rCall|+tCall|+cCall|+gCall|+pCall|+prCall)   [(fiveK rN tN cN gN pN,pTfr)]
  && assertStateP (five tN cN gN pN   trRN find' r    ldR    ldT ldC ldG ldP  ldPr) (rCall|+tCall|+cCall|+gCall|+pCall|+prCall)   [(fiveK rN tN cN gN pN,pTrr)]
  && assertStateP (five cN gN pN trRN trN  find' t    ldT    ldC ldG ldP ldPr ldTr) (tCall|+cCall|+gCall|+pCall|+prCall|+trCall)  [(fiveK tN cN gN pN trRN,tr)]
  
  && assertStateP (five rN tN cN gN   pN   findUnambiguous' root ldRoot ldR ldT ldC ldG  ldP)  (rootCall|+rCall|+tCall|+cCall|+gCall|+pCall) (Just p) 
  && assertStateP (five tN cN gN pN   sRN  findUnambiguous' r    ldR    ldT ldC ldG ldP  ldPr) (rCall|+tCall|+cCall|+gCall|+pCall|+prCall)   (Just pSr)
  && assertStateP (five tN cN gN pN   tfRN findUnambiguous' r    ldR    ldT ldC ldG ldP  ldPr) (rCall|+tCall|+cCall|+gCall|+pCall|+prCall)   (Just pTfr)
  && assertStateP (five tN cN gN pN   trRN findUnambiguous' r    ldR    ldT ldC ldG ldP  ldPr) (rCall|+tCall|+cCall|+gCall|+pCall|+prCall)   (Just pTrr)
  && assertStateP (five cN gN pN trRN trN  findUnambiguous' t    ldT    ldC ldG ldP ldPr ldTr) (tCall|+cCall|+gCall|+pCall|+prCall|+trCall)  (Just tr)
  
prop_six rootN rN tN cN gN pN trN sKey sContent tfKey tfContent = 
 "" `notElem` [rootN,rN,tN,cN,gN,pN,trN,sKey,sContent,tfKey,tfContent] ==>  
  let tr = make trN
      p = setSubmitRepo (setTrainFileRepo (setTrainRunRepo (make pN) trr) tfr) sr
      g = addChild (make gN) p
      c = addChild (make cN) g
      t = addChild (make tN) c
      r = addChild (make rN) t
      root = addChild (make rootN :: Root) r
      
      sr = addSubmit emptySubmitRepo sKey sContent
      tfr = addTrainFile emptyTrainFileRepo tfKey tfContent
      trr = addChild emptyTrainRunRepo tr
      
      pSr = makeProjectSubmitRepo sr
      pTfr = makeProjectTrainFileRepo tfr
      pTrr = makeProjectTrainRunRepo trr
      
      prs k = [(k,pSr),(k,pTfr),(k,pTrr)]
  
  in assertStateP (six' find' root ldRoot ldR ldT ldC ldG ldP  ldPr) (rootCall|+rCall|+tCall|+cCall|+gCall|+pCall|+pr3Call) (prs $ sixK rootN rN tN cN gN pN)
  && assertStateP (six' find' r    ldR    ldT ldC ldG ldP ldPr ldTr) (rCall|+tCall|+cCall|+gCall|+pCall|+pr3Call|+trCall)   [(sixK rN tN cN gN pN trRN,tr)]
  
  && assertStateP (six' findUnambiguous' r    ldR    ldT ldC ldG ldP ldPr ldTr) (rCall|+tCall|+cCall|+gCall|+pCall|+pr3Call|+trCall) (Just tr)
  
  && assertStateP (six rN tN cN gN pN   sRN  find' root ldRoot ldR ldT ldC ldG ldP  ldPr) (rootCall|+rCall|+tCall|+cCall|+gCall|+pCall|+prCall) [(sixK rootN rN tN cN gN pN,pSr)]
  && assertStateP (six rN tN cN gN pN   tfRN find' root ldRoot ldR ldT ldC ldG ldP  ldPr) (rootCall|+rCall|+tCall|+cCall|+gCall|+pCall|+prCall) [(sixK rootN rN tN cN gN pN,pTfr)]
  && assertStateP (six rN tN cN gN pN   trRN find' root ldRoot ldR ldT ldC ldG ldP  ldPr) (rootCall|+rCall|+tCall|+cCall|+gCall|+pCall|+prCall) [(sixK rootN rN tN cN gN pN,pTrr)]
  && assertStateP (six tN cN gN pN trRN trN  find' r    ldR    ldT ldC ldG ldP ldPr ldTr) (rCall|+tCall|+cCall|+gCall|+pCall|+prCall|+trCall)   [(sixK rN tN cN gN pN trRN,tr)]
  
  && assertStateP (six rN tN cN gN pN   sRN  findUnambiguous' root ldRoot ldR ldT ldC ldG ldP  ldPr) (rootCall|+rCall|+tCall|+cCall|+gCall|+pCall|+prCall) (Just pSr)
  && assertStateP (six rN tN cN gN pN   tfRN findUnambiguous' root ldRoot ldR ldT ldC ldG ldP  ldPr) (rootCall|+rCall|+tCall|+cCall|+gCall|+pCall|+prCall) (Just pTfr)
  && assertStateP (six rN tN cN gN pN   trRN findUnambiguous' root ldRoot ldR ldT ldC ldG ldP  ldPr) (rootCall|+rCall|+tCall|+cCall|+gCall|+pCall|+prCall) (Just pTrr)
  && assertStateP (six tN cN gN pN trRN trN  findUnambiguous' r    ldR    ldT ldC ldG ldP ldPr ldTr) (rCall|+tCall|+cCall|+gCall|+pCall|+prCall|+trCall)   (Just tr)
  
prop_seven rootN rN tN cN gN pN trN sKey sContent tfKey tfContent = 
 "" `notElem` [rootN,rN,tN,cN,gN,pN,trN,sKey,sContent,tfKey,tfContent] ==>  
  let tr = make trN
      p = setSubmitRepo (setTrainFileRepo (setTrainRunRepo (make pN) trr) tfr) sr
      g = addChild (make gN) p
      c = addChild (make cN) g
      t = addChild (make tN) c
      r = addChild (make rN) t
      root = addChild (make rootN :: Root) r
      
      sr = addSubmit emptySubmitRepo sKey sContent
      tfr = addTrainFile emptyTrainFileRepo tfKey tfContent
      trr = addChild emptyTrainRunRepo tr
        
  in assertStateP (seven' find'            root ldRoot ldR ldT ldC ldG ldP ldPr ldTr) (rootCall|+rCall|+tCall|+cCall|+gCall|+pCall|+pr3Call|+trCall) [(sevenK rootN rN tN cN gN pN trRN,tr)]
  && assertStateP (seven' findUnambiguous' root ldRoot ldR ldT ldC ldG ldP ldPr ldTr) (rootCall|+rCall|+tCall|+cCall|+gCall|+pCall|+pr3Call|+trCall) (Just tr)
  
  && assertStateP (seven rN tN cN gN pN trRN trN find'            root ldRoot ldR ldT ldC ldG ldP ldPr ldTr) (rootCall|+rCall|+tCall|+cCall|+gCall|+pCall|+prCall|+trCall) [(sevenK rootN rN tN cN gN pN trRN,tr)]
  && assertStateP (seven rN tN cN gN pN trRN trN findUnambiguous' root ldRoot ldR ldT ldC ldG ldP ldPr ldTr) (rootCall|+rCall|+tCall|+cCall|+gCall|+pCall|+prCall|+trCall) (Just tr)
  


{-
prop_findGeneral rootName ns = let names = filter (not.null) $ nub ns in names /= [] ==>
  let root = makeNode rootName 4 7 names
  in  all (uncurry (presentOnFind root)) $ makeKeys root

presentOnFind :: Node -> Node -> [String] -> Bool                                                                                                      
presentOnFind rt n [roo]                 = let z = (zero,n)                   in getName n == roo && [z] == find zero rt                                                                                       && Just n == findUnambiguous zero rt                    
presentOnFind rt n [roo,r]               = let z = (one roo,n)                in getName n == r   && [z] == find (oneM r) rt                 && all (z `elem`) (map (`find`rt) $ partialOne r)                 && Just n == findUnambiguous (oneM r) rt                
presentOnFind rt n [roo,r,t]             = let z = (two roo r,n)              in getName n == t   && [z] == find (twoM r t) rt               && all (z `elem`) (map (`find`rt) $ partialTwo r t)               && Just n == findUnambiguous (twoM r t) rt              
presentOnFind rt n [roo,r,t,c]           = let z = (three roo r t,n)          in getName n == c   && [z] == find (threeM r t c) rt           && all (z `elem`) (map (`find`rt) $ partialThree r t c)           && Just n == findUnambiguous (threeM r t c) rt          
presentOnFind rt n [roo,r,t,c,g]         = let z = (four roo r t c,n)         in getName n == g   && [z] == find (fourM r t c g) rt          && all (z `elem`) (map (`find`rt) $ partialFour r t c g)          && Just n == findUnambiguous (fourM r t c g) rt         
presentOnFind rt n [roo,r,t,c,g,p]       = let z = (five roo r t c g,n)       in getName n == p   && [z] == find (fiveM r t c g p) rt        && all (z `elem`) (map (`find`rt) $ partialFive r t c g p)        && Just n == findUnambiguous (fiveM r t c g p) rt       
presentOnFind rt n [roo,r,t,c,g,p,pr]    = let z = (six roo r t c g p,n)      in getName n == pr  && [z] == find (sixM r t c g p pr) rt      && all (z `elem`) (map (`find`rt) $ partialSix r t c g p pr)      && Just n == findUnambiguous (sixM r t c g p pr) rt     
presentOnFind rt n [roo,r,t,c,g,p,pr,tr] = let z = (seven roo r t c g p pr,n) in getName n == tr  && [z] == find (sevenM r t c g p pr tr) rt && all (z `elem`) (map (`find`rt) $ partialSeven r t c g p pr tr) && Just n == findUnambiguous (sevenM r t c g p pr tr) rt

partialOne a               = [S x Z | x <- [Just a, Nothing]]
partialTwo a b             = [S x s | x <- [Just a, Nothing], s <- partialOne b]
partialThree a b c         = [S x s | x <- [Just a, Nothing], s <- partialTwo b c]
partialFour a b c d        = [S x s | x <- [Just a, Nothing], s <- partialThree b c d]
partialFive a b c d e      = [S x s | x <- [Just a, Nothing], s <- partialFour b c d e]
partialSix a b c d e f     = [S x s | x <- [Just a, Nothing], s <- partialFive b c d e f]
partialSeven a b c d e f g = [S x s | x <- [Just a, Nothing], s <- partialSix b c d e f g]

makeNode :: String -> Int -> Int -> [String] -> Node
makeNode rootName nbChildren maxDepth nodeNames = let (Just result,_) = f maxDepth (rootName:nodeNames) in result
 where f _ []     = (Nothing,[])
       f 0 (x:xs) = (Just $ make x, xs)
       f d (x:xs) = let (result,rest) = g nbChildren xs $ make x
                        g 0  ns m = (m,ns)
                        g nb ns m = let g' = g (nb-1) ns'; (c,ns') = f (d-1) ns in g' $ maybe m (addChild m) c
                    in  (Just result,rest)

makeKeys :: Node -> [(Node,[String])]
makeKeys = map (second reverse) . f []
 where f parentKey n = let key = getName n:parentKey
                           p = (n,key)
                           ps = concatMap (f key) $ getChildren n
                       in  (p:ps)

one' = S Nothing zero
two' = S Nothing one'
three' = S Nothing two'
four' = S Nothing three'
five' = S Nothing four'
six' = S Nothing five'
seven' = S Nothing six'

zero = Z
one a               = K a zero
two a b             = K b $ one a
three a b c         = K c $ two a b
four a b c d        = K d $ three a b c
five a b c d e      = K e $ four a b c d
six a b c d e f     = K f $ five a b c d e
seven a b c d e f g = K g $ six a b c d e f

oneM a               = S (Just a) zero
twoM a b             = S (Just a) $ oneM b
threeM a b c         = S (Just a) $ twoM b c
fourM a b c d        = S (Just a) $ threeM b c d
fiveM a b c d e      = S (Just a) $ fourM b c d e
sixM a b c d e f     = S (Just a) $ fiveM b c d e f
sevenM a b c d e f g = S (Just a) $ sixM b c d e f g


-}