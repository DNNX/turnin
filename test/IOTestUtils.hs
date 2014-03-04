module IOTestUtils where

import System.IO.Temp
import Control.Monad
import System.Directory
import Control.Exception (bracket)
import System.Random

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
randString minSize maxSize
 | minSize > maxSize = error $ "RandString: Min size [" ++ show minSize ++ "] should be less than or equal to max size [" ++ show maxSize ++ "]"
 | otherwise = do
  l <- randInt minSize maxSize
  f l []
 where f 0 acc = return acc
       f i acc = do rest <- f (i-1) acc
                    pos <- randInt 0 $ length stringDomain - 1
                    return $ stringDomain !! pos : rest
  
  
randListS :: Int -> Int -> Int -> Int -> IO [String]
randListS minSize maxSize minStringSize maxStringSize
 | minSize > maxSize = error $ "RandListS: Min size [" ++ show minSize ++ "] should be less than or equal to max size [" ++ show maxSize ++ "]"
 | minStringSize > maxStringSize = error $ "RandListS: Min string size [" ++ show minStringSize ++ "] should be less than or equal to max string size [" ++ show maxStringSize ++ "]"
 | otherwise = do
  l <- randInt minSize maxSize
  f l []
 where f 0 acc = return acc
       f i acc = do rest <- f (i-1) acc
                    s <- unique rest
                    return $ s:rest
           
       unique ss = do
        s <- randString minStringSize maxStringSize
        if s `elem` ss then unique ss else return s
  
  