module Loader
(
	load
)

where

import FileSystem
import Path

import System.IO
import System.FilePath
import System.Directory

selfRefering :: [String]
selfRefering = [Path.this, Path.parentOnly]

data ElememtType = File | Folder

load :: FilePath -> IO (FileSystem Bool)
load = undefined

createObjects :: FileSystem Bool -> FilePath -> FileSystem Bool
createObjects = undefined

bindTypes :: FilePath -> IO ElememtType
bindTypes path = do
	dir <- doesDirectoryExist path
	if dir then
		return Folder
	else 
		return File

handleContents :: 
	FileSystem Bool -> FilePath -> 
	(Either [FilePath] String) -> IO (FileSystem Bool)
handleContents r path (Left dirContents) = 
	monadFold 
		(return afterFolder)
		dirContents
		(\b -> \p -> bindTypes p >>= (contents p)  >>= (handleContents b p) )
	where
		folderPath = fromString rootPath $ dropFileName path
		folderName = takeFileName path
		afterFolder = folder r folderPath folderName
handleContents r path (Right fileContents) = 
	return $ file r filePath fileName fileContents
	where	
		filePath = fromString rootPath $ dropFileName path
		fileName = takeFileName path
	
monadFold :: (Monad m) => m a -> [b] -> (a -> b -> m a) -> m a
monadFold b [] _ = b
monadFold b (x:xs) f = b >>= (\bv -> f bv x)
		
contents :: FilePath -> ElememtType -> IO (Either [FilePath] String)
contents path File = 
		readFile path >>=
		(return . Right)
contents path Folder = 
		return path >>= 
		getDirectoryContents >>= 
		(return . Left . map (\e -> joinPath [path, e]) . filter (not . flip elem selfRefering))

