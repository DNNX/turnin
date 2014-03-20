{-# LANGUAGE  FlexibleInstances, UndecidableInstances #-}
{-# OPTIONS_GHC -F -pgmF htfpp -fno-warn-incomplete-patterns #-}
module Infrastructure.Finder.FinderCompileTest where

import Test.Framework
import Control.Monad.State

import Infrastructure.Node
import Domain.Root
import Domain.Repo
import Domain.Term
import Domain.Course
import Domain.Group
import Domain.Project
import Domain.ProjectRepo
import Domain.TrainRun

import Infrastructure.Finder

{-# ANN module "HLint: ignore Use camelCase" #-}
{-# ANN module "HLint: ignore Reduce duplication" #-}
{-# ANN module "HLint: ignore Evaluate" #-}
{-# ANN module "HLint: ignore Use ++" #-}

test_zeroRoot          = assertState (zero find' root ldRoot) rootCall [(zeroK, root)]
test_zeroRepo          = assertState (zero find' r    ldR)    rCall    [(zeroK, r)]
test_zeroTerm          = assertState (zero find' t    ldT)    tCall    [(zeroK, t)]
test_zeroCourse        = assertState (zero find' c    ldC)    cCall    [(zeroK, c)]
test_zeroGroup         = assertState (zero find' g    ldG)    gCall    [(zeroK, g)]
test_zeroProject       = assertState (zero find' p    ldP)    pCall    [(zeroK, p)]
test_zeroSubmitRepo    = assertState (zero find' pSr  ldPr)   prCall   [(zeroK, pSr)]
test_zeroTrainFileRepo = assertState (zero find' pTfr ldPr)   prCall   [(zeroK, pTfr)]
test_zeroTrainRunRepo  = assertState (zero find' pTrr ldPr)   prCall   [(zeroK, pTrr)]
test_zeroTrainRun      = assertState (zero find' tr   ldTr)   trCall   [(zeroK, tr)]

test_oneRoot         = assertState (one find' root ldRoot ldR)  (rootCall|+rCall) [(oneK rootN,r)]
test_oneRepo         = assertState (one find' r    ldR    ldT)  (rCall|+tCall)    [(oneK rN  ,t)]
test_oneTerm         = assertState (one find' t    ldT    ldC)  (tCall|+cCall)    [(oneK tN  ,c)]
test_oneCourse       = assertState (one find' c    ldC    ldG)  (cCall|+gCall)    [(oneK cN  ,g)]
test_oneGroup        = assertState (one find' g    ldG    ldP)  (gCall|+pCall)    [(oneK gN  ,p)]
test_oneProject      = assertState (one find' p    ldP    ldPr) (pCall|+pr3Call)  [(pk ,pSr),(pk, pTfr),(pk, pTrr)] where pk = oneK pN
test_oneTrainRunRepo = assertState (one find' pTrr ldPr   ldTr) (prCall|+trCall)  [(oneK trRN ,tr)]

test_twoRoot    = assertState (two find' root ldRoot ldR  ldT)  (rootCall|+rCall|+tCall) [(twoK rootN rN ,t)]
test_twoRepo    = assertState (two find' r    ldR    ldT  ldC)  (rCall|+tCall|+cCall)    [(twoK rN tN ,c)]
test_twoTerm    = assertState (two find' t    ldT    ldC  ldG)  (tCall|+cCall|+gCall)    [(twoK tN cN ,g)]
test_twoCourse  = assertState (two find' c    ldC    ldG  ldP)  (cCall|+gCall|+pCall)    [(twoK cN gN ,p)]
test_twoGroup   = assertState (two find' g    ldG    ldP  ldPr) (gCall|+pCall|+pr3Call)  [(prk ,pSr),(prk, pTfr),(prk, pTrr)] where prk = twoK gN pN
test_twoProject = assertState (two find' p    ldP    ldPr ldTr) (pCall|+pr3Call|+trCall) [(twoK pN trRN,tr)]

test_threeRoot   = assertState (three find' root ldRoot ldR ldT  ldC)  (rootCall|+rCall|+tCall|+cCall) [(threeK rootN rN tN,c)]
test_threeRepo   = assertState (three find' r    ldR    ldT ldC  ldG)  (rCall|+tCall|+cCall|+gCall)    [(threeK rN tN cN,g)]
test_threeTerm   = assertState (three find' t    ldT    ldC ldG  ldP)  (tCall|+cCall|+gCall|+pCall)    [(threeK tN cN gN,p)]
test_threeCourse = assertState (three find' c    ldC    ldG ldP  ldPr) (cCall|+gCall|+pCall|+pr3Call)  [(prk ,pSr),(prk, pTfr),(prk, pTrr)] where prk = threeK cN gN pN
test_threeGroup  = assertState (three find' g    ldG    ldP ldPr ldTr) (gCall|+pCall|+pr3Call|+trCall) [(threeK gN pN trRN,tr)]

test_fourRoot   = assertState (four find' root ldRoot ldR ldT ldC  ldG)  (rootCall|+rCall|+tCall|+cCall|+gCall) [(fourK rootN rN tN cN,g)]
test_fourRepo   = assertState (four find' r    ldR    ldT ldC ldG  ldP)  (rCall|+tCall|+cCall|+gCall|+pCall)    [(fourK rN tN cN gN,p)]
test_fourCourse = assertState (four find' t    ldT    ldC ldG ldP  ldPr) (tCall|+cCall|+gCall|+pCall|+pr3Call)  [(prk ,pSr),(prk, pTfr),(prk, pTrr)] where prk = fourK tN cN gN pN
test_fourGroup  = assertState (four find' c    ldC    ldG ldP ldPr ldTr) (cCall|+gCall|+pCall|+pr3Call|+trCall) [(fourK cN gN pN trRN,tr)]

test_fiveRoot = assertState (five find' root ldRoot ldR ldT ldC ldG  ldP)  (rootCall|+rCall|+tCall|+cCall|+gCall|+pCall) [(fiveK rootN rN tN cN gN,p)]
test_fiveRepo = assertState (five find' r    ldR    ldT ldC ldG ldP  ldPr) (rCall|+tCall|+cCall|+gCall|+pCall|+pr3Call)  [(prk ,pSr),(prk, pTfr),(prk, pTrr)] where prk = fiveK rN tN cN gN pN
test_fiveTerm = assertState (five find' t    ldT    ldC ldG ldP ldPr ldTr) (tCall|+cCall|+gCall|+pCall|+pr3Call|+trCall) [(fiveK tN cN gN pN trRN,tr)]

test_sixRoot = assertState (six find' root ldRoot ldR ldT ldC ldG ldP  ldPr) (rootCall|+rCall|+tCall|+cCall|+gCall|+pCall|+pr3Call) [(prk ,pSr),(prk, pTfr),(prk, pTrr)] where prk = sixK rootN rN tN cN gN pN
test_sixRepo = assertState (six find' r    ldR    ldT ldC ldG ldP ldPr ldTr) (rCall|+tCall|+cCall|+gCall|+pCall|+pr3Call|+trCall)   [(sixK rN tN cN gN pN trRN,tr)]

test_sevenRoot = assertState (seven find' root ldRoot ldR ldT ldC ldG ldP ldPr ldTr) (rootCall|+rCall|+tCall|+cCall|+gCall|+pCall|+pr3Call|+trCall)   [(sevenK rootN rN tN cN gN pN trRN,tr)]

test_unambiguousZeroRoot          = assertState (zero findUnambiguous' root ldRoot) rootCall (Just root)
test_unambiguousZeroRepo          = assertState (zero findUnambiguous' r    ldR)    rCall    (Just r)
test_unambiguousZeroTerm          = assertState (zero findUnambiguous' t    ldT)    tCall    (Just t)
test_unambiguousZeroCourse        = assertState (zero findUnambiguous' c    ldC)    cCall    (Just c)
test_unambiguousZeroGroup         = assertState (zero findUnambiguous' g    ldG)    gCall    (Just g)
test_unambiguousZeroProject       = assertState (zero findUnambiguous' p    ldP)    pCall    (Just p)
test_unambiguousZeroSubmitRepo    = assertState (zero findUnambiguous' pSr  ldPr)   prCall   (Just pSr)
test_unambiguousZeroTrainFileRepo = assertState (zero findUnambiguous' pTfr ldPr)   prCall   (Just pTfr)
test_unambiguousZeroTrainRunRepo  = assertState (zero findUnambiguous' pTrr ldPr)   prCall   (Just pTrr)
test_unambiguousZeroTrainRun      = assertState (zero findUnambiguous' tr   ldTr)   trCall   (Just tr)

test_unambiguousOneRoot         = assertState (one findUnambiguous' root ldRoot ldR)  (rootCall|+rCall) (Just r)
test_unambiguousOneRepo         = assertState (one findUnambiguous' r    ldR    ldT)  (rCall|+tCall)    (Just t)
test_unambiguousOneTerm         = assertState (one findUnambiguous' t    ldT    ldC)  (tCall|+cCall)    (Just c)
test_unambiguousOneCourse       = assertState (one findUnambiguous' c    ldC    ldG)  (cCall|+gCall)    (Just g)
test_unambiguousOneGroup        = assertState (one findUnambiguous' g    ldG    ldP)  (gCall|+pCall)    (Just p)
test_unambiguousOneTrainRunRepo = assertState (one findUnambiguous' pTrr ldPr   ldTr) (prCall|+trCall)  (Just tr)

test_unabmiguousTwoRoot    = assertState (two findUnambiguous' root ldRoot ldR  ldT)  (rootCall|+rCall|+tCall) (Just t)
test_unabmiguousTwoRepo    = assertState (two findUnambiguous' r    ldR    ldT  ldC)  (rCall|+tCall|+cCall)    (Just c)
test_unabmiguousTwoTerm    = assertState (two findUnambiguous' t    ldT    ldC  ldG)  (tCall|+cCall|+gCall)    (Just g)
test_unabmiguousTwoCourse  = assertState (two findUnambiguous' c    ldC    ldG  ldP)  (cCall|+gCall|+pCall)    (Just p)
test_unabmiguousTwoProject = assertState (two findUnambiguous' p    ldP    ldPr ldTr) (pCall|+pr3Call|+trCall) (Just tr)

test_unambiguousThreeRoot   = assertState (three findUnambiguous' root ldRoot ldR ldT  ldC)  (rootCall|+rCall|+tCall|+cCall) (Just c)
test_unambiguousThreeRepo   = assertState (three findUnambiguous' r    ldR    ldT ldC  ldG)  (rCall|+tCall|+cCall|+gCall)    (Just g)
test_unambiguousThreeTerm   = assertState (three findUnambiguous' t    ldT    ldC ldG  ldP)  (tCall|+cCall|+gCall|+pCall)    (Just p)
test_unambiguousThreeGroup  = assertState (three findUnambiguous' g    ldG    ldP ldPr ldTr) (gCall|+pCall|+pr3Call|+trCall) (Just tr)

test_unambiguousFourRoot   = assertState (four findUnambiguous' root ldRoot ldR ldT ldC  ldG)  (rootCall|+rCall|+tCall|+cCall|+gCall) (Just g)
test_unambiguousFourRepo   = assertState (four findUnambiguous' r    ldR    ldT ldC ldG  ldP)  (rCall|+tCall|+cCall|+gCall|+pCall)    (Just p)
test_unambiguousFourGroup  = assertState (four findUnambiguous' c    ldC    ldG ldP ldPr ldTr) (cCall|+gCall|+pCall|+pr3Call|+trCall) (Just tr)

test_unambiguousFiveRoot = assertState (five findUnambiguous' root ldRoot ldR ldT ldC ldG  ldP)  (rootCall|+rCall|+tCall|+cCall|+gCall|+pCall) (Just p)
test_unambiguousFiveTerm = assertState (five findUnambiguous' t    ldT    ldC ldG ldP ldPr ldTr) (tCall|+cCall|+gCall|+pCall|+pr3Call|+trCall) (Just tr)

test_unambiguousSixRepo = assertState (six findUnambiguous' r    ldR    ldT ldC ldG ldP ldPr ldTr) (rCall|+tCall|+cCall|+gCall|+pCall|+pr3Call|+trCall) (Just tr)

test_unambiguousSevenRoot = assertState (seven findUnambiguous' root ldRoot ldR ldT ldC ldG ldP ldPr ldTr) (rootCall|+rCall|+tCall|+cCall|+gCall|+pCall|+pr3Call|+trCall) (Just tr)


-- Utils
assertState actual expectedV expectedK = assertEqual (expectedV,expectedK) actual

zero  func x a                 = func x (ld0 a)  Z
one   func x a b               = func x (ld1 b)  (sn (ld0 a) Z)
two   func x a b c'            = func x (ld2 c') (sn (ld0 a) $ sn (ld1 b) Z)
three func x a b c' d          = func x (ld3 d)  (sn (ld0 a) $ sn (ld1 b) $ sn (ld2 c') Z)
four  func x a b c' d e        = func x (ld4 e)  (sn (ld0 a) $ sn (ld1 b) $ sn (ld2 c') $ sn (ld3 d) Z)
five  func x a b c' d e f      = func x (ld5 f)  (sn (ld0 a) $ sn (ld1 b) $ sn (ld2 c') $ sn (ld3 d) $ sn (ld4 e) Z)
six   func x a b c' d e f g'   = func x (ld6 g') (sn (ld0 a) $ sn (ld1 b) $ sn (ld2 c') $ sn (ld3 d) $ sn (ld4 e) $ sn (ld5 f) Z)
seven func x a b c' d e f g' h = func x (ld7 h)  (sn (ld0 a) $ sn (ld1 b) $ sn (ld2 c') $ sn (ld3 d) $ sn (ld4 e) $ sn (ld5 f) $ sn (ld6 g') Z)

zeroK                  = Z
oneK a                 = K a zeroK
twoK a b               = K b  $ oneK a
threeK a b c'          = K c' $ twoK a b
fourK a b c' d         = K d  $ threeK a b c'
fiveK a b c' d e       = K e  $ fourK a b c' d
sixK a b c' d e f      = K f  $ fiveK a b c' d e
sevenK a b c' d e f g' = K g' $ sixK a b c' d e f

(a0,a1,a2,a3,a4,a5,a6,a7) |+ (b0,b1,b2,b3,b4,b5,b6,b7) = (a0+b0,a1+b1,a2+b2,a3+b3,a4+b4,a5+b5,a6+b6,a7+b7)
find' x f s = let (a,b) = runState (find s f x) noCalls in (b,a)
findUnambiguous' x f s = let (a,b) = runState (findUnambiguous s f x) noCalls in (b,a)
sn = S Nothing

type T = State (Int,Int,Int,Int,Int,Int,Int,Int)
ld0 :: (Z -> a -> T a) -> Z -> a -> T a
ld0 f = f

ld1 :: (K Z -> a -> T a) -> K Z -> a -> T a
ld1 f = f

ld2 :: (K (K Z) -> a -> T a) -> K (K Z) -> a -> T a
ld2 f = f

ld3 :: (K (K (K Z)) -> a -> T a) -> K (K (K Z)) -> a -> T a
ld3 f = f

ld4 :: (K (K (K (K Z))) -> a -> T a) -> K (K (K (K Z))) -> a -> T a
ld4 f = f

ld5 :: (K (K (K (K (K Z)))) -> a -> T a) -> K (K (K (K (K Z)))) -> a -> T a
ld5 f = f

ld6 :: (K (K (K (K (K (K Z))))) -> a -> T a) -> K (K (K (K (K (K Z))))) -> a -> T a
ld6 f = f

ld7 :: (K (K (K (K (K (K (K Z)))))) -> a -> T a) -> K (K (K (K (K (K (K Z)))))) -> a -> T a
ld7 f = f

ldRoot :: k -> Root -> T Root
ldRoot _ x = do
  (rootX,rX,tX,cX,gX,pX,prX,trX) <- get
  put (rootX+1,rX,tX,cX,gX,pX,prX,trX)
  return x

ldR :: k -> Repo -> T Repo
ldR _ x = do
  (rootX,rX,tX,cX,gX,pX,prX,trX) <- get
  put (rootX,rX+1,tX,cX,gX,pX,prX,trX)
  return x

ldT :: k -> Term -> T Term
ldT _ x = do
  (rootX,rX,tX,cX,gX,pX,prX,trX) <- get
  put (rootX,rX,tX+1,cX,gX,pX,prX,trX)
  return x

ldC :: k -> Course -> T Course
ldC _ x = do
  (rootX,rX,tX,cX,gX,pX,prX,trX) <- get
  put (rootX,rX,tX,cX+1,gX,pX,prX,trX)
  return x

ldG :: k -> Group -> T Group
ldG _ x = do
  (rootX,rX,tX,cX,gX,pX,prX,trX) <- get
  put (rootX,rX,tX,cX,gX+1,pX,prX,trX)
  return x

ldP :: k -> Project -> T Project
ldP _ x = do
  (rootX,rX,tX,cX,gX,pX,prX,trX) <- get
  put (rootX,rX,tX,cX,gX,pX+1,prX,trX)
  return x

ldPr :: k -> ProjectRepo -> T ProjectRepo
ldPr _ x = do
  (rootX,rX,tX,cX,gX,pX,prX,trX) <- get
  put (rootX,rX,tX,cX,gX,pX,prX+1,trX)
  return x

ldTr :: k -> TrainRun -> T TrainRun
ldTr _ x = do
  (rootX,rX,tX,cX,gX,pX,prX,trX) <- get
  put (rootX,rX,tX,cX,gX,pX,prX,trX+1)
  return x

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

noCalls  = (0,0,0,0,0,0,0,0)
rootCall = (1,0,0,0,0,0,0,0)
rCall    = (0,1,0,0,0,0,0,0)
tCall    = (0,0,1,0,0,0,0,0)
cCall    = (0,0,0,1,0,0,0,0)
gCall    = (0,0,0,0,1,0,0,0)
pCall    = (0,0,0,0,0,1,0,0)
prCall   = (0,0,0,0,0,0,1,0)
trCall   = (0,0,0,0,0,0,0,1)
pr3Call = prCall |+ prCall |+ prCall






