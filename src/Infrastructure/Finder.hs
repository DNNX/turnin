{-# LANGUAGE FunctionalDependencies, FlexibleInstances, UndecidableInstances #-}
module Infrastructure.Finder
( find
, findUnambiguous
, Z(Z)
, S(S)
, K(K)
, T(T)
) where

import Infrastructure.Node

data Z = Z                        deriving (Show,Eq)
data S f s = S (Maybe String) f s deriving (Show,Eq)
data K k = K String k             deriving (Show,Eq)

class Find s m a b | s m a -> b                           where find_ :: s -> a -> m b
instance (Monad m, HasNode a) => Find Z m [(k,a)] [(k,a)] where find_ Z = return
instance (Monad m, HasNode a, HasNode (ChildType a), Find s m [(K k1,ChildType a)] [(K k2,b)]) =>
 Find (S (k1 -> a -> m a) s) m [(k1,a)] [(K k2, b)] where 
  find_ (S x loadF s) ps = do
    loadedParents <- mapM (uncurry loadF) ps
    let func k parent = [ (K (getName parent) k, child) | child <- filter (matchesCriteria x) $ getChildren parent ] 
        cs = concatMap (uncurry func) $ zip (map fst ps) loadedParents
    find_ s cs

data T t = T t
instance Monad T where
 return = T
 (T t) >>= f = f t
 
find s x = let (T t) = find_ s [(Z,x)] in t 
         
matchesCriteria Nothing  _  = True
matchesCriteria (Just s) n  = s == getName n

findUnambiguous s x = case find s x of
                       [] -> Nothing
                       [(_,y)] -> Just y
                       _   -> error "Finder::findUnambiguous: Not implemented for ambiguous results"