{-# LANGUAGE FunctionalDependencies, FlexibleInstances, UndecidableInstances #-}
module Infrastructure.Finder
( find
, Z(Z)
, S(S)
) where

import Infrastructure.Node

data Z = Z                    deriving (Show,Eq)
data S s = S (Maybe String) s deriving (Show,Eq)

class Size s where
instance Size Z where
instance Size s => Size (S s) where

class HasNode a => Find s a b | s a -> b where find :: s -> a -> b
instance HasNode a => Find Z a [(Z,a)]       where find Z x = [(Z,x)]
instance (HasNode a, Find s (ChildType a) [(s,b)]) => Find (S s) a [(S s, b)] where 
 find (S x s) p = concatMap f $ filter (matchesCriteria x) $ getChildren p
  where f n = [(S (Just $ getName p) k,b) | (k,b) <- find s n]
                                
matchesCriteria Nothing  _  = True
matchesCriteria (Just s) n  = s == getName n

