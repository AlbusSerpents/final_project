module FileSystem
(
	FileSystem,
	focus,
	changeFocus,
	getFolderContents,
	changeResult,
	root,
	file,
	folder,
	removeElement,
	writeToFile,
	appendToFile,
	readFromFile,
	result,
	exists
)

where

import qualified Path as P
import Data.Maybe (fromJust, isNothing, isJust, listToMaybe, fromMaybe, mapMaybe)
import Data.Function (on)
import Data.Either (Either(Left), Either(Right))
import Data.List (intersperse)

data FileSystem a = Root {rootDir :: FileSystemElement, focus :: P.Path, result :: a}
data FileSystemElement = 
	File {name :: P.Name, text :: String} | 
	Folder {name :: P.Name, children :: [FileSystemElement]}

type Action = (FileSystemElement -> Maybe FileSystemElement)		
	
instance Show FileSystemElement where
	show (File n c) =  show n ++ " " ++ show c
	show (Folder n c) =  show n ++ " " ++ show c

instance Show (FileSystem a) where
	show = show . rootDir
	
changeFocus :: FileSystem a -> P.Path -> FileSystem Bool
changeFocus r@(Root rd focus _) path  
	| found && isFolder target = Root rd path True
	| otherwise = Root rd focus False
	where
		target = find r path
		found = isJust target
		isFolder (Just (Folder _ _)) = True
		isFolder _ = False

changeResult :: FileSystem a -> b -> FileSystem b
changeResult (Root rd focus res) newRes = Root rd focus newRes 

root :: FileSystem Bool
root = Root (Folder P.root []) (P.rootPath) True

file :: FileSystem a -> P.Path -> P.Name -> String -> FileSystem Bool
file r p fileName fileContent = addElement r p $ File fileName fileContent
	
folder :: FileSystem a -> P.Path -> P.Name -> FileSystem Bool
folder r p folderName = addElement r p $ Folder folderName []
	
addElement :: FileSystem a -> P.Path -> FileSystemElement -> FileSystem Bool
addElement r path newElement = executeAction r parentPath (`add` newElement)
	where
		parentPath = P.parents path
		add (File _ _) _ = Nothing
		add (Folder n c) new = Just $ Folder n (new:c)

removeElement :: FileSystem a -> P.Path -> FileSystem Bool
removeElement r path 
	| path == P.rootPath = root
	| path == focus r = Root (rootDir newRoot) P.rootPath True
	| otherwise = newRoot
	where
		parentPath = P.parents path
		elementName = P.title path
		newRoot = executeAction r parentPath (`delete` elementName)
		delete (File _ _) _ = Nothing
		delete (Folder n c) dName = 
			Just $ Folder n $ deleteBy (\e -> name e == dName) c

deleteBy :: (FileSystemElement -> Bool) -> [FileSystemElement] -> [FileSystemElement]
deleteBy f [] = []
deleteBy f (x:xs)
	| f x = deleteBy f xs
	| otherwise = x:(deleteBy f xs)			

exists :: FileSystem a -> P.Path -> FileSystem Bool
exists r = changeResult r . isJust . find r
	
readFromFile :: FileSystem a -> P.Path -> FileSystem (Bool, Maybe String)
readFromFile r path = maybeToEither r $ return path >>= (find r) >>= readFrom
		where
			readFrom (Folder _ _) = Nothing
			readFrom (File _ text) = Just text
		
writeToFile :: FileSystem a -> P.Path -> String -> FileSystem Bool
writeToFile r path text = executeAction r path (`write` text)
	where
		write (Folder _ _) _ = Nothing
		write (File n c) text = Just $ File n text	
	
appendToFile :: FileSystem a -> P.Path-> String -> FileSystem Bool
appendToFile r path textToAppend = 
	fromMaybe (changeResult r False) $ 
		return path >>= 
		(find r) >>= 
		(\f -> appendText f textToAppend) >>= 
		(\t -> return $ writeToFile r path t)
	where
		appendText (Folder _ _) _ = Nothing
		appendText (File n text) moreText = Just $ text ++ moreText

getFolderContents :: FileSystem a -> P.Path -> FileSystem (Bool, Maybe String)
getFolderContents r p = maybeToEither r $ return p >>= (find r) >>= getChildren
	where
		getChildren (File _ _) = Nothing
		getChildren (Folder n children) = Just $ concat $ intersperse ", " $ map name children

maybeToEither :: FileSystem a -> Maybe b -> FileSystem (Bool, Maybe b)
maybeToEither r Nothing = changeResult r $ (False, Nothing)
maybeToEither r folderContents = changeResult r $ (True, folderContents)
		
executeAction :: FileSystem a -> P.Path -> Action -> FileSystem Bool
executeAction r path action = replace r path $ return path >>= (find r) >>= action
		
replace :: FileSystem a -> P.Path -> Maybe FileSystemElement -> FileSystem Bool
replace r _ Nothing = changeResult r False
replace (Root rd focus res) p (Just e) = Root newRd focus True
	where
		names = P.unroot p
		newRd = replaceElements rd names e

replaceElements :: FileSystemElement -> [P.Name] -> FileSystemElement -> FileSystemElement
replaceElements f [] new = new
replaceElements f@(File _ _) _ new = f
replaceElements folder@(Folder name children) (n:[]) new
	| hasChild folder n = Folder name $ replaceChild new children
	| otherwise = folder
replaceElements folder@(Folder name children) (n:ns) new
	| hasChild folder n = Folder name $ map (\f -> replaceElements f ns new) children
	| otherwise = folder

hasChild :: FileSystemElement -> P.Name -> Bool
hasChild (File _ _) _ = False
hasChild (Folder _ children) targetName = elem targetName $ map name children

find :: FileSystem a -> P.Path -> Maybe FileSystemElement
find (Root rd focus res) p
	| P.unroot p == [] = Just rd 
	| otherwise = recursiveFind rd $ P.content p
		
recursiveFind :: FileSystemElement -> [P.Name] -> Maybe FileSystemElement
recursiveFind f [] = Just f
recursiveFind f@(File name _) (n:ns)
	| name == n && ns == [] = Just f
	| otherwise = Nothing
recursiveFind f@(Folder name children) (n:ns)
	| n == name && ns == [] = Just f
	| n == name = listToMaybe $ mapMaybe (flip recursiveFind ns) children
	| otherwise = Nothing
	
replaceChild :: FileSystemElement -> [FileSystemElement] -> [FileSystemElement]
replaceChild _ [] = []
replaceChild new (x:xs)
	| on (==) name new x = new:xs
	| otherwise = x:(replaceChild new xs)