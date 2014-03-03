module Infrastructure.Persister
( save
, load
, delete
) where

import System.IO
import System.Directory
import Control.Monad
import System.FilePath
import Control.Applicative

type Key = [String]

save :: Key -> String -> IO ()
save []  _       = return ()
save key content = let path = joinPath key in do
  buildPath path
  withFile path WriteMode $ \h -> hPutStr h content

load :: Key -> IO (Maybe String)
load [] = return Nothing
load key = let path = joinPath key in do
  fileExists <- doesFileExist path
  if not fileExists then return Nothing else Just <$> readFile path

delete :: Key -> IO ()
delete []  = return ()
delete key = let path = joinPath key in do
  isFile <- doesFileExist path
  isDir <- doesDirectoryExist path
  if isFile then removeFile path else when isDir $ removeDirectoryRecursive path
    
buildPath path = forM_ (allParentPaths path) $ \subPath -> do
  exists <- doesDirectoryExist subPath
  unless exists $ createDirectory subPath
  
allParentPaths path = f path
 where f ""  = []
       f "/" = []
       f "." = []
       f p   = let dir = takeDirectory p
               in  dir : f dir