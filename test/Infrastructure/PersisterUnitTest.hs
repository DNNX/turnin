{-# OPTIONS_GHC -F -pgmF htfpp -fno-warn-incomplete-patterns#-}
module Infrastructure.PersisterUnitTest where

import Test.Framework
import IOTestUtils
import System.Directory
import System.FilePath

import Infrastructure.Node
import Infrastructure.Persister

{-# ANN module "HLint: ignore Use camelCase" #-}

test_saveLoad = inTmpDir $ do
  let n0 = make "node"
      n1 = setCache n0 "cacheKey" "cacheValue"
      n2 = setConfig n1 "configKey" "configValue"
      n3 = addChild n2 $ make "child"
      n4 = setCache (setConfig (addChild n0 (make "child2")) "configKey" "configValue2") "cacheKey" "cacheValue2"
  rootKey <- getRootKey  
  notFound <- load rootKey n0
  assertEqual Nothing notFound
                    
  success <- save rootKey n3
  assertBool success
  
  n <- load rootKey n0
  assertEqual (Just n3) n
  
  success' <- save rootKey n4
  assertBool success'
  
  n' <- load rootKey n0
  assertEqual (Just n4) n'
  
getRootKey = do
  dir <- getCurrentDirectory
  let rootKey = splitPath dir
  return rootKey