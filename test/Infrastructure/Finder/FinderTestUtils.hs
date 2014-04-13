{-# OPTIONS_GHC -F -pgmF htfpp -fno-warn-incomplete-patterns #-}
module Infrastructure.Finder.FinderTestUtils where

import Test.Framework
import Control.Monad.State

import Domain.Root
import Domain.Repo
import Domain.Term
import Domain.Course
import Domain.Group
import Domain.Project
import Domain.ProjectRepo
import Domain.TrainRun

import Infrastructure.Finder

assertState  actual expectedV expectedK = assertEqual (expectedV,expectedK) actual
assertStateP actual expectedV expectedK = (expectedV,expectedK) == actual

zero   func x a                 = func x (ld0 a)  Z
one'   func x a b               = func x (ld1 b)  (sn (ld0 a) Z)
two'   func x a b c'            = func x (ld2 c') (sn (ld0 a) $ sn (ld1 b) Z)
three' func x a b c' d          = func x (ld3 d)  (sn (ld0 a) $ sn (ld1 b) $ sn (ld2 c') Z)
four'  func x a b c' d e        = func x (ld4 e)  (sn (ld0 a) $ sn (ld1 b) $ sn (ld2 c') $ sn (ld3 d) Z)
five'  func x a b c' d e f      = func x (ld5 f)  (sn (ld0 a) $ sn (ld1 b) $ sn (ld2 c') $ sn (ld3 d) $ sn (ld4 e) Z)
six'   func x a b c' d e f g'   = func x (ld6 g') (sn (ld0 a) $ sn (ld1 b) $ sn (ld2 c') $ sn (ld3 d) $ sn (ld4 e) $ sn (ld5 f) Z)
seven' func x a b c' d e f g' h = func x (ld7 h)  (sn (ld0 a) $ sn (ld1 b) $ sn (ld2 c') $ sn (ld3 d) $ sn (ld4 e) $ sn (ld5 f) $ sn (ld6 g') Z)

one   h1                   func x a b               = func x (ld1 b)  (sj h1 (ld0 a) Z)
two   h1 h2                func x a b c'            = func x (ld2 c') (sj h1 (ld0 a) $ sj h2 (ld1 b) Z)
three h1 h2 h3             func x a b c' d          = func x (ld3 d)  (sj h1 (ld0 a) $ sj h2 (ld1 b) $ sj h3 (ld2 c') Z)
four  h1 h2 h3 h4          func x a b c' d e        = func x (ld4 e)  (sj h1 (ld0 a) $ sj h2 (ld1 b) $ sj h3 (ld2 c') $ sj h4 (ld3 d) Z)
five  h1 h2 h3 h4 h5       func x a b c' d e f      = func x (ld5 f)  (sj h1 (ld0 a) $ sj h2 (ld1 b) $ sj h3 (ld2 c') $ sj h4 (ld3 d) $ sj h5 (ld4 e) Z)
six   h1 h2 h3 h4 h5 h6    func x a b c' d e f g'   = func x (ld6 g') (sj h1 (ld0 a) $ sj h2 (ld1 b) $ sj h3 (ld2 c') $ sj h4 (ld3 d) $ sj h5 (ld4 e) $ sj h6 (ld5 f) Z)
seven h1 h2 h3 h4 h5 h6 h7 func x a b c' d e f g' h = func x (ld7 h)  (sj h1 (ld0 a) $ sj h2 (ld1 b) $ sj h3 (ld2 c') $ sj h4 (ld3 d) $ sj h5 (ld4 e) $ sj h6 (ld5 f) $ sj h7 (ld6 g') Z)

oneP   h1                   func x a b               = func x (ld1 b)  (S h1 nd (ld0 a) Z)
twoP   h1 h2                func x a b c'            = func x (ld2 c') (S h1 nd (ld0 a) $ S h2 nd (ld1 b) Z)
threeP h1 h2 h3             func x a b c' d          = func x (ld3 d)  (S h1 nd (ld0 a) $ S h2 nd (ld1 b) $ S h3 nd (ld2 c') Z)
fourP  h1 h2 h3 h4          func x a b c' d e        = func x (ld4 e)  (S h1 nd (ld0 a) $ S h2 nd (ld1 b) $ S h3 nd (ld2 c') $ S h4 nd (ld3 d) Z)
fiveP  h1 h2 h3 h4 h5       func x a b c' d e f      = func x (ld5 f)  (S h1 nd (ld0 a) $ S h2 nd (ld1 b) $ S h3 nd (ld2 c') $ S h4 nd (ld3 d) $ S h5 nd (ld4 e) Z)
sixP   h1 h2 h3 h4 h5 h6    func x a b c' d e f g'   = func x (ld6 g') (S h1 nd (ld0 a) $ S h2 nd (ld1 b) $ S h3 nd (ld2 c') $ S h4 nd (ld3 d) $ S h5 nd (ld4 e) $ S h6 nd (ld5 f) Z)
sevenP h1 h2 h3 h4 h5 h6 h7 func x a b c' d e f g' h = func x (ld7 h)  (S h1 nd (ld0 a) $ S h2 nd (ld1 b) $ S h3 nd (ld2 c') $ S h4 nd (ld3 d) $ S h5 nd (ld4 e) $ S h6 nd (ld5 f) $ S h7 nd (ld6 g') Z)

sn = S Nothing nd
sj h = S (Just h) nd

nd = error "FinderTestUtils::nb: Should not be called"

zeroK                  = Z
oneK a                 = K a zeroK
twoK a b               = K b  $ oneK a
threeK a b c'          = K c' $ twoK a b
fourK a b c' d         = K d  $ threeK a b c'
fiveK a b c' d e       = K e  $ fourK a b c' d
sixK a b c' d e f      = K f  $ fiveK a b c' d e
sevenK a b c' d e f g' = K g' $ sixK a b c' d e f

(a0,a1,a2,a3,a4,a5,a6,a7) |+ (b0,b1,b2,b3,b4,b5,b6,b7) = (a0+b0,a1+b1,a2+b2,a3+b3,a4+b4,a5+b5,a6+b6,a7+b7)
find' x f s = findP x f s noCalls
findUnambiguous' x f s = findUnambiguousP x f s noCalls

findP x f s z = let (a,b) = runState (find s f x) z in (b,a)
findUnambiguousP x f s z = let (a,b) = runState (findUnambiguous s f x) z in (b,a) 

type T = State (Int,Int,Int,Int,Int,Int,Int,Int)
ld0 :: Monad m => (Z -> a -> m a) -> Z -> a -> m a
ld0 f = f

ld1 :: Monad m => (K Z -> a -> m a) -> K Z -> a -> m a
ld1 f = f

ld2 :: Monad m => (K (K Z) -> a -> m a) -> K (K Z) -> a -> m a
ld2 f = f

ld3 :: Monad m => (K (K (K Z)) -> a -> m a) -> K (K (K Z)) -> a -> m a
ld3 f = f

ld4 :: Monad m => (K (K (K (K Z))) -> a -> m a) -> K (K (K (K Z))) -> a -> m a
ld4 f = f

ld5 :: Monad m => (K (K (K (K (K Z)))) -> a -> m a) -> K (K (K (K (K Z)))) -> a -> m a
ld5 f = f

ld6 :: Monad m => (K (K (K (K (K (K Z))))) -> a -> m a) -> K (K (K (K (K (K Z))))) -> a -> m a
ld6 f = f

ld7 :: Monad m => (K (K (K (K (K (K (K Z)))))) -> a -> m a) -> K (K (K (K (K (K (K Z)))))) -> a -> m a
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

noCalls  = (0,0,0,0,0,0,0,0) :: (Int,Int,Int,Int,Int,Int,Int,Int)
rootC = (1,0,0,0,0,0,0,0) :: (Int,Int,Int,Int,Int,Int,Int,Int)
rC    = (0,1,0,0,0,0,0,0) :: (Int,Int,Int,Int,Int,Int,Int,Int)
tC    = (0,0,1,0,0,0,0,0) :: (Int,Int,Int,Int,Int,Int,Int,Int)
cC    = (0,0,0,1,0,0,0,0) :: (Int,Int,Int,Int,Int,Int,Int,Int)
gC    = (0,0,0,0,1,0,0,0) :: (Int,Int,Int,Int,Int,Int,Int,Int)
pC    = (0,0,0,0,0,1,0,0) :: (Int,Int,Int,Int,Int,Int,Int,Int)
prC   = (0,0,0,0,0,0,1,0) :: (Int,Int,Int,Int,Int,Int,Int,Int)
trC   = (0,0,0,0,0,0,0,1) :: (Int,Int,Int,Int,Int,Int,Int,Int)
pr3C = prC |+ prC |+ prC


