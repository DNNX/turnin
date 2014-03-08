{-# OPTIONS_GHC -F -pgmF htfpp -fno-warn-incomplete-patterns#-}
module Infrastructure.PersisterUnitTest where

import Test.Framework
import IOTestUtils
import Control.Monad
import Data.Maybe

import Infrastructure.Node
import Infrastructure.Persister

{-# ANN module "HLint: ignore Use camelCase" #-}
{-# ANN module "HLint: ignore Reduce duplication" #-}

test_saveLoadSingleNode = inTmpDir $ let rootName = "root" in do
  let root0 = setConfig (setCache (addChild (make rootName) (addChild (make "child") (make "grandChild"))) "cacheKey" "cacheValue") "configKey" "configValue" 
      root1 = setConfig (setCache (addChild (make rootName)           (make "child")                     ) "cacheKey" "cacheValue") "configKey" "configValue"
      root2 = setConfig (setCache (addChild (make rootName)           (make "child")                     ) "cacheKey" "cacheValue1") "configKey" "configValue1"
  rootKey <- getRootKey
  notFound <- load rootKey rootName
  assertEqual Nothing notFound
  
  forM_ ["idem", "potency"] $ \_ -> do
    s <- save ["/" | null rootKey] root0
    assertBool $ not s
                      
    success <- save rootKey root0
    assertBool success
    
    n <- load rootKey rootName
    assertEqual (Just root1) n
    
    success' <- save rootKey root2
    assertBool success'
    
    n' <- load rootKey rootName
    assertEqual (Just root2) n'

    
test_saveLoadTree = inTmpDir $ do
  let rootName = "rootName"
      nbChildren = 2 :: Int
      maxDepth = 2 :: Int
      n0 = makeNodeTree rootName nbChildren maxDepth
      n1 = makeDifferentNodeTree rootName nbChildren maxDepth
  
  rootKey <- getRootKey
  notFound <- load rootKey rootName
  assertEqual Nothing notFound
  
  forM_ ["idem"] $ \_ -> do
    s <- save ["/" | null rootKey] n0
    assertBool $ not s
                      
    success <- save rootKey n0
    assertBool success
    
    n <- load rootKey rootName
    assertNodeEqualLocalRecursive rootKey n0 $ fromJust n
    
    success' <- save rootKey n1
    assertBool success'
    
    n' <- load rootKey rootName
    
    assertNodeEqualLocalRecursive rootKey n1 $ fromJust n'
  
assertNodeEqualLocalRecursive key expected actual = do
  assertEqual (getName expected) (getName actual)
  assertEqual (getConfigPairs expected) (getConfigPairs actual)
  assertEqual (getCachePairs expected) (getCachePairs actual)
  assertEqual (map getName $ getChildren expected) (map getName $ getChildren actual) -- Children are not loaded
  forM_ (getChildren actual) $ \child -> do
    assertEqual [] $ getConfigPairs child
    assertEqual [] $ getCachePairs child
    assertEqual [] $ getChildren child  
  forM_ (getChildren expected) $ \expectedChild -> let key' = key ++ [getName expected] in do                                                 
    actualChild <- load key' $ getName expectedChild
    assertNodeEqualLocalRecursive key' expectedChild $ fromJust actualChild
    
makeNodeTree s _ 0 = make s
makeNodeTree s n d = let n0 = foldl (\acc (k,v) -> setConfig acc (s++"configKey"++k) v) (make s) [("1","v1"),("2","v2"),("3","v3")]
                         n1 = foldl (\acc (k,v) -> setCache acc (s++"cacheKey"++k) v) n0 [("1","v1"),("2","v2"),("3","v3")]
                     in       foldl (\acc k -> addChild acc (makeNodeTree (s++"c"++k) (n-1) (d-1))) n1 $ take n $ map show ([1..] :: [Int])
                     
makeDifferentNodeTree s _ 0 = make s
makeDifferentNodeTree s n d = let n0 = foldl (\acc (k,v) -> setConfig acc (s++"configKey"++k) v) (make s) [("2","v2"),("3","v3"),("4","v4")]
                                  n1 = foldl (\acc (k,v) -> setCache acc (s++"cacheKey"++k) v) n0 [("2","v2"),("3","v3"),("4","v4")]
                               in      foldl (\acc k -> addChild acc (makeDifferentNodeTree (s++"c"++k) (n-1) (d-1))) n1 $ take n $ map show ([2..] :: [Int])                     
                      