{-# LANGUAGE FunctionalDependencies, FlexibleInstances, UndecidableInstances #-}
module Infrastructure.Finder
( find
, findUnambiguous
, Z(Z)
, S(S)
, K(K)
) where

import Infrastructure.Node

data Z = Z                    deriving (Show,Eq)
data S s = S (Maybe String) s deriving (Show,Eq)
data K k = K String k         deriving (Show,Eq)

class Find s a b | s a -> b                    where find_ :: s -> a -> b
instance (HasNode a) => Find Z [(k,a)] [(k,a)] where find_ Z = id
instance (HasNode a, HasNode (ChildType a), Find s [(K k1,ChildType a)] [(K k2,b)]) =>
 Find (S s) [(k1,a)] [(K k2, b)] where 
  find_ (S x s) ps = find_ s $ concatMap func ps
   where func (k,parent) = let cs = filter (matchesCriteria x) $ getChildren parent
                           in  [ (K (getName parent) k, child) | child <- cs ]

find s x = find_ s [(Z,x)] 
         
matchesCriteria Nothing  _  = True
matchesCriteria (Just s) n  = s == getName n

findUnambiguous s x = case find s x of
                       [] -> Nothing
                       [(_,y)] -> Just y
                       _   -> error "Finder::findUnambiguous: Not implemented for ambiguous results"