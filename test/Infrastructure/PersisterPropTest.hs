{-# OPTIONS_GHC -F -pgmF htfpp -fno-warn-incomplete-patterns#-}
module Infrastructure.PersisterPropTest where

import Test.Framework
import IOTestUtils

import Infrastructure.Persister

{-# ANN module "HLint: ignore Use camelCase" #-}

test_saveAndLoading = testRepeat $ inTmpDir $ do
  [folder1, folder2, folder3, value1, value2, value3] <- randListS 6 6 0 100
  let key1 = [  ] ++ [folder1]
      key2 = key1 ++ [folder2]
      key3 = key2 ++ [folder3]
        
      valueKey1 = key1 ++ [value1]
      valueKey2 = key2 ++ [value2]
      valueKey3 = key3 ++ [value3] 
      
      [content1, content2, content3] = map toContent [valueKey1, valueKey2, valueKey3]
  
  save valueKey1 content1
  save valueKey2 content2
  save valueKey3 content3
  
  assertLoadedValues [ (valueKey1, Just content1)
                     , (valueKey2, Just content2)
                     , (valueKey3, Just content3)
                     ]
  b1 <- randBool
  if b1 then do 
    delete key1 
    assertLoadedValues [ (valueKey1, Nothing)
                       , (valueKey2, Nothing)
                       , (valueKey3, Nothing)
                       ] else do 
    delete valueKey1     
    assertLoadedValues [ (valueKey1, Nothing)
                       , (valueKey2, Just content2)
                       , (valueKey3, Just content3)
                       ]
    b2 <- randBool
    if b2 then do
      delete key2
      assertLoadedValues [ (valueKey2, Nothing)
                         , (valueKey3, Nothing)
                         ] else do
      delete valueKey2     
      assertLoadedValues [ (valueKey2, Nothing)
                         , (valueKey3, Just content3)
                         ]
                         
      b3 <- randBool 
      delete $ if b3 then key3 else valueKey3
      assertLoadedValues [ (valueKey3, Nothing) ]
      
    
toContent ss = unwords ss ++" content"   
    
assertLoadedValues [] = return ()
assertLoadedValues ((k,ev):rest) = do
  v' <- load k
  assertEqual ev v'
  assertLoadedValues rest
  
