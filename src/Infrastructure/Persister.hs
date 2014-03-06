module Infrastructure.Persister
( save
, load
) where

import System.Directory
import System.FilePath
import Control.Monad
import Data.List
import Data.List.Split
import Data.Maybe

import Infrastructure.Node

type Key = [String]
type FileName = String

save :: Key -> Node -> IO Bool
save key node = let dirPath = joinPath key; nodePath = combine dirPath $ getName node in do
  exists <- doesDirectoryExist dirPath
  if not exists then return False else do
  nodeExists <- doesDirectoryExist nodePath
  (if nodeExists then updateNode else createNode) nodePath node
  return True
  
load :: Key -> FileName -> IO (Maybe Node)
load key nodeName = let nodeKey = key ++ [nodeName]; nodePath = joinPath nodeKey in do
  nodeExists <- doesDirectoryExist nodePath
  if not nodeExists then return Nothing else do
  loadedNode <- getFilesAndDirectories nodePath >>= buildNodeCacheAndChildren nodeKey
  return $ Just $ moveConfigFromCache loadedNode
  
updateNode :: FilePath -> Node -> IO ()
updateNode path node = do
  (_,dirs) <- getFilesAndDirectories path
  writeFile (combine path configFileName) $ serializeConfig $ getConfigPairs node
  let (p1a2Children, a1p2Children) = listDiff dirs $ map getName $ getChildren node -- present in 1 but absent from 2, absent from 1 but present in 2
  forM_ (getCachePairs node) $ \(k,v) -> writeFile (combine path k) v
  forM_ p1a2Children $ \k -> removeDirectoryRecursive (combine path k)
  forM_ a1p2Children $ \k -> createNode (combine path k) $ fromJust $ getChild node k
  
createNode :: FilePath -> Node -> IO ()
createNode path node = let n = moveConfigToCache node in do
  createDirectory path
  putStrLn $ "Path: " ++ show path
  forM_ (getCachePairs n) $ \(key,content) -> writeFile (combine path key) content
  forM_ (getChildren n) $ \child -> createNode (combine path $ getName child) child

listDiff :: Eq a => [a] -> [a] -> ([a],[a]) -- xs -> ys -> ('Present in xs but absent in ys', 'Absent in xs but present in ys')
listDiff xs ys = (filter (not.(`elem` ys)) xs, filter (not.(`elem` xs)) ys)
  
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
serializeConfig = unlines . map (\(x,y) -> x ++ "=" ++ y)

deserializeConfig :: String -> [(String,String)]
deserializeConfig raw = f [] $ lines raw
 where f acc [] = acc
       f acc (l:ls) = let (key:value) = splitOn "=" l
                      in  f ((key, intercalate "=" value):acc) ls

configFileName = ".config"