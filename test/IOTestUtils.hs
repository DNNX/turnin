module IOTestUtils where

import System.IO.Temp
import Control.Exception
import System.Random

import System.Directory
import System.FilePath
import Control.Monad
import Control.Applicative
import Data.List

import Infrastructure.Node

nbRepeats = 100

repeatTest = replicateM_ nbRepeats :: IO () -> IO ()

inTmpDir f = withSystemTempDirectory "tmp" $ \tempPath -> bracket
 (do previous <- getCurrentDirectory; setCurrentDirectory tempPath; return previous)
 setCurrentDirectory
 (const f)

randBool = randomIO :: IO Bool

randInt :: Int -> Int -> IO Int
randInt minVal maxVal
 | minVal > maxVal =  error $ "RandInt: Min val [" ++ show minVal ++ "] should be less than or equal to max val [" ++ show maxVal ++ "]"
 | minVal == maxVal = return minVal
 | otherwise = do
  i <- randomIO :: IO Int
  return $ i `mod` (maxVal - minVal) + minVal
  
stringDomain = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
  
randString :: Int -> Int -> IO String
randString minSize maxS
 | minSize > maxS = error $ "RandString: Min size [" ++ show minSize ++ "] should be less than or equal to max size [" ++ show maxS ++ "]"
 | otherwise = do
  l <- randInt minSize maxS
  f l []
 where f 0 acc = return acc
       f i acc = do rest <- f (i-1) acc
                    pos <- randInt 0 $ length stringDomain - 1
                    return $ stringDomain !! pos : rest
  
  
randListS :: Int -> Int -> Int -> Int -> IO [String]
randListS minS maxS minStringSize maxStringSize
 | minS > maxS = error $ "RandListS: Min size [" ++ show minS ++ "] should be less than or equal to max size [" ++ show maxS ++ "]"
 | minStringSize > maxStringSize = error $ "RandListS: Min string size [" ++ show minStringSize ++ "] should be less than or equal to max string size [" ++ show maxStringSize ++ "]"
 | otherwise = do
  l <- randInt minS maxS
  f l []
 where f 0 acc = return acc
       f i acc = do rest <- f (i-1) acc
                    s <- unique rest
                    return $ s:rest
           
       unique ss = do
        s <- randString minStringSize maxStringSize
        if s `elem` ss then unique ss else return s
  
randSubset :: [a] -> IO [a]
randSubset [] = return []
randSubset (x:xs) = do
  rest <- randSubset xs 
  b <- randBool
  return $ [x | b] ++ rest
  
randNodeTree s n d = addRandConfig (make s) >>= addRandCache >>= addRandChildren n d
addRandConfig = addRandMappings setConfig   
addRandCache = addRandMappings setCache
addRandMappings f n = randListS 0 10 0 10 >>= foldM g n
 where g acc key = f acc key <$> randString 0 10
 
addRandChildren 0 _ p = return p
addRandChildren _ 0 p = return p 
addRandChildren n d p = randListS n n 0 100 >>= foldM f p
 where f acc childName = liftM (addChild acc) $ randNodeTree childName (n-1) (d-1)

modifyTree p n d = addRandConfig p >>= addRandCache >>= addRandChildren n d >>=
                    removeRandConfig >>= removeRandCache >>= removeRandChildren >>=
                    modifyRandConfig >>= modifyRandCache >>= modifyRandChildren
                    
removeRandConfig = removeRandMappings (map fst . getConfigPairs) unsetConfig
removeRandCache = removeRandMappings (map fst . getCachePairs) unsetCache
removeRandChildren = removeRandMappings (map getName . getChildren) removeChild
removeRandMappings getKeysF unsetF n = foldM f n $ getKeysF n
 where f acc k = randBool >>= \b -> return $ if b then unsetF acc k else acc
 
modifyRandConfig = modifyRandMappings (map fst . getConfigPairs) setConfig
modifyRandCache = modifyRandMappings (map fst . getCachePairs) setCache
modifyRandMappings getKeysF setF n = foldM f n $ getKeysF n
 where f acc k = do
        b <- randBool
        v <- randString 0 100
        return $ if b then setF acc k v else acc
        
modifyRandChildren n = foldM f n $ getChildren n
 where f acc child = do
        b <- randBool
        child' <- modifyRandChildren child
        if b then return acc else do
          let n' = removeChild n $ getName child
          return $ addChild n' child'
        
modifyChildren :: Int -> Int -> Node -> IO Node       
modifyChildren n d p = do
  ls <- randListS n n 0 100 
  let newChildren = ls \\ map getName (getChildren p)
  node' <- foldM g p newChildren
  namesToRemove <- randSubset $ map getName $ getChildren node'
  foldM h node' namesToRemove
 where g acc name = liftM (addChild acc) $ randNodeTree name (n -1) (d -1)
       h acc name = return $ removeChild acc name

getRootKey = splitPath <$> getCurrentDirectory
  