{-# LANGUAGE FunctionalDependencies, FlexibleInstances, UndecidableInstances, FlexibleContexts #-}
module Infrastructure.Finder
( find
, findUnambiguous
, disambiguate
, Z(Z)
, S(S)
, K(K)
) where

import Infrastructure.Node
import Control.Monad (liftM)

data Z = Z                            deriving (Show,Eq)
data K k = K String k                 deriving (Show,Eq)
data S d l s = S (Maybe String) d l s

class Find s m a b | s m a -> b                           where find_ :: s -> a -> m b
instance (Monad m, HasNode a) => Find Z m [(k,a)] [(k,a)] where find_ Z = return
instance (Monad m, HasNode a, HasNode (ChildType a), Find s m [(K k1,ChildType a)] [(K k2,b)]) =>
 Find (S d (k1 -> a -> m a) s) m [(k1,a)] [(K k2, b)] where 
  find_ (S x _ loadF s) ps = do
    loadedParents <- load loadF ps
    let func k parent = [ (K (getName parent) k, child) | child <- filter (matchesCriteria x) $ getChildren parent ] 
    find_ s $ concatMap (uncurry func) loadedParents

find s loadF x = find_ s [(Z,x)] >>= load loadF
load f ns = liftM (zip (map fst ns)) $ mapM (uncurry f) ns
         
matchesCriteria Nothing  _  = True
matchesCriteria (Just s) n  = s == getName n

findUnambiguous s loadF x = do
  result <- find s loadF x
  case result of
    []      -> return Nothing
    [(_,y)] -> return $ Just y
    _       -> do s' <- disambiguate s $ map fst result
                  findUnambiguous s' loadF x

disambiguate _ _ = error "Finder::disambiguate: Not implemented"