module Infrastructure.Persister
( save
, load
) where

import System.Directory
import System.FilePath
import Control.Monad
import Data.List
import Data.List.Split

import Infrastructure.Node

type Key = [String]
type FileName = String

save :: Key -> Node -> IO Bool
save key node = let dirPath = joinPath key; nodeKey = key ++ [getName node]; nodePath = joinPath nodeKey in do
  exists <- doesDirectoryExist dirPath
  if not exists then return False else do
  nodeExists <- doesDirectoryExist nodePath
  if nodeExists then updateNode nodeKey node else createNode nodePath node
  return True
  
load :: Key -> FileName -> IO (Maybe Node)
load key nodeName = let nodeKey = key ++ [nodeName]; nodePath = joinPath nodeKey in do
  nodeExists <- doesDirectoryExist nodePath
  if not nodeExists then return Nothing else do
  loadedNode <- getFilesAndDirectories nodePath >>= buildNodeCacheAndChildren nodeKey
  return $ Just $ moveConfigFromCache loadedNode
  
updateNode :: Key -> Node -> IO ()
updateNode key node = let path = joinPath key; n = moveConfigToCache node in do
  (files,dirs) <- getFilesAndDirectories path
  let cacheToRemove    = elemsToRemove [configFileName] files $ map fst $ getCachePairs n
      childrenToRemove = elemsToRemove [] dirs $ map getName $ getChildren n

--  putStrLn $ "files: " ++ show files
--  putStrLn $ "dir: " ++ show dirs
--  putStrLn $ "cacheToRemove: " ++ show cacheToRemove
--  putStrLn $ "childrenToRemove: " ++ show childrenToRemove
--  putStrLn ""
      
  forM_ cacheToRemove $ \k -> removeFile $ combine path k
  forM_ childrenToRemove $ \k -> removeDirectoryRecursive $ combine path k
  forM_ (getCachePairs n) $ \(k,v) -> writeFile (combine path k) v
  forM_ (getChildren n) $ \child -> save key child
  
createNode :: FilePath -> Node -> IO ()
createNode path node = let n = moveConfigToCache node in do
  createDirectory path
  forM_ (getCachePairs n) $ \(key,content) -> writeFile (combine path key) content
  forM_ (getChildren n) $ \child -> createNode (combine path $ getName child) child

elemsToRemove :: Eq a => [a] -> [a] -> [a] -> [a] -- ignores -> xs -> ys -> 'Present in xs but absent in ys AND absent in ignores'
elemsToRemove ignores xs ys = filter (not.(`elem` ignores ++ ys)) xs
  
getFilesAndDirectories :: FilePath -> IO ([FileName],[FileName])
getFilesAndDirectories path = getDirectoryContents path >>= f [] []
 where f a1 a2 []     = return (a1,a2)
       f a1 a2 (fileName:fs) = let fPath = combine path fileName in 
        if fileName `elem` [".",".."] then f a1 a2 fs else do
        isFile <- doesFileExist fPath
        isDir <- doesDirectoryExist fPath
        if isFile then f (fileName:a1) a2 fs else
         if isDir  then f a1 (fileName:a2) fs else 
          error $ "File ["++fPath++"] should exist and either be a file or a directory"
  
buildNodeCacheAndChildren :: Key -> ([FileName],[FileName]) -> IO Node
buildNodeCacheAndChildren nodeKey (files,dirs) = foldM f (make $ last nodeKey) files >>= \n -> foldM g n dirs
 where f acc fileName = liftM (setCache acc fileName) $ readFile (joinPath $ nodeKey ++ [fileName])
       g acc dirName  = return $ addChild acc $ make dirName

moveConfigToCache :: Node -> Node
moveConfigToCache node = setCache node configFileName $ serializeConfig $ getConfigPairs node
moveConfigFromCache node = 
  let configPairs = deserializeConfig $ getCache node configFileName
      f acc (k,v) = setConfig acc k v
  in  foldl f (unsetCache node configFileName) configPairs

serializeConfig :: [(String,String)] -> String
serializeConfig [] = emptyConfigFlag
serializeConfig ps = unlines $ map (\(x,y) -> x ++ "=" ++ y) ps

deserializeConfig :: String -> [(String,String)]
deserializeConfig raw
 | raw == emptyConfigFlag = []
 | otherwise              = f [] $ lines raw
 where f acc [] = acc
       f acc (l:ls) = let (key:value) = splitOn "=" l
                      in  f ((key, intercalate "=" value):acc) ls

emptyConfigFlag = "EMPTY_CONFIG"
configFileName = ".config"