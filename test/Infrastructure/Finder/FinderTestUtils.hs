{-# OPTIONS_GHC -F -pgmF htfpp -fno-warn-incomplete-patterns #-}
module Infrastructure.Finder.FinderTestUtils where

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

assertState actual expectedV expectedK = assertEqual (expectedV,expectedK) actual

zero  func x a                 = func x (ld0 a)  Z
one'   func x a b               = func x (ld1 b)  (sn (ld0 a) Z)
two'   func x a b c'            = func x (ld2 c') (sn (ld0 a) $ sn (ld1 b) Z)
three' func x a b c' d          = func x (ld3 d)  (sn (ld0 a) $ sn (ld1 b) $ sn (ld2 c') Z)
four'  func x a b c' d e        = func x (ld4 e)  (sn (ld0 a) $ sn (ld1 b) $ sn (ld2 c') $ sn (ld3 d) Z)
five'  func x a b c' d e f      = func x (ld5 f)  (sn (ld0 a) $ sn (ld1 b) $ sn (ld2 c') $ sn (ld3 d) $ sn (ld4 e) Z)
six'   func x a b c' d e f g'   = func x (ld6 g') (sn (ld0 a) $ sn (ld1 b) $ sn (ld2 c') $ sn (ld3 d) $ sn (ld4 e) $ sn (ld5 f) Z)
seven' func x a b c' d e f g' h = func x (ld7 h)  (sn (ld0 a) $ sn (ld1 b) $ sn (ld2 c') $ sn (ld3 d) $ sn (ld4 e) $ sn (ld5 f) $ sn (ld6 g') Z)

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

noCalls  = (0,0,0,0,0,0,0,0) :: (Int,Int,Int,Int,Int,Int,Int,Int)
rootCall = (1,0,0,0,0,0,0,0) :: (Int,Int,Int,Int,Int,Int,Int,Int)
rCall    = (0,1,0,0,0,0,0,0) :: (Int,Int,Int,Int,Int,Int,Int,Int)
tCall    = (0,0,1,0,0,0,0,0) :: (Int,Int,Int,Int,Int,Int,Int,Int)
cCall    = (0,0,0,1,0,0,0,0) :: (Int,Int,Int,Int,Int,Int,Int,Int)
gCall    = (0,0,0,0,1,0,0,0) :: (Int,Int,Int,Int,Int,Int,Int,Int)
pCall    = (0,0,0,0,0,1,0,0) :: (Int,Int,Int,Int,Int,Int,Int,Int)
prCall   = (0,0,0,0,0,0,1,0) :: (Int,Int,Int,Int,Int,Int,Int,Int)
trCall   = (0,0,0,0,0,0,0,1) :: (Int,Int,Int,Int,Int,Int,Int,Int)
pr3Call = prCall |+ prCall |+ prCall


