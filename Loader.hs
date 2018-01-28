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

data ElementType = File {filePath :: FilePath, context :: FilePath} | 
	Folder {folderPath :: FilePath, context :: FilePath} deriving Show 
type Contents = Either [FilePath] String
type ElementContext = (ElementType, Contents)

load :: FilePath -> IO (FileSystem Bool)
load = undefined

createObjects :: FileSystem Bool -> FilePath -> FileSystem Bool
createObjects = undefined

bindTypes :: (FilePath, FilePath) -> IO ElementType
bindTypes (path, context) = do
	dir <- doesDirectoryExist $ joinPath [context, path]
	if dir then
		return $ Folder path context
	else 
		return $ File path context

-- monadFold still doesn't work --
handleContents :: FileSystem Bool -> ElementContext -> IO (FileSystem Bool)
handleContents r (element, (Left dirContents)) = 
	monadFold 
		(return afterFolder)
		dirContents
		(\b -> \p -> bindTypes (p, context element) >>= contents >>= (handleContents b) )
	where
		path = folderPath element
		folderLocation = elementPath path
		folderName = elementName path
		afterFolder = folder r folderLocation folderName
handleContents r (element, (Right fileContents)) = 
	return $ file r fileLocation fileName fileContents
	where	
		path = filePath element
		fileLocation = elementPath path
		fileName = elementName path
	
elementPath :: FilePath -> Path
elementPath = fromString rootPath . dropFileName

elementName :: FilePath -> Name
elementName = takeFileName	
	
monadFold :: (Monad m) => m a -> [b] -> (a -> b -> m a) -> m a
monadFold b [] _ = b
monadFold b (x:xs) f = b >>= (\bv -> f bv x)
		
contents :: ElementType -> IO ElementContext
contents f@(File path context) = 
	(readFile $ toRealWorldPath f) >>= 
	(return . pair f . Right)
contents f@(Folder path context) = 
		return fullPath >>= 
		getDirectoryContents >>= 
			(return . pair f . Left . 
			map (\e -> joinPath [path, e]) . 
			filter (not . flip elem selfRefering))
		where
			fullPath = toRealWorldPath f

toRealWorldPath :: ElementType -> FilePath
toRealWorldPath (File fp ctx) = joinPath [ctx, fp]
toRealWorldPath (Folder fp ctx) = joinPath [ctx, fp]
			
pair :: ElementType -> Contents -> ElementContext
pair p c = (p, c)