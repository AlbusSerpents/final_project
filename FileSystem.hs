module FileSystem
(
	FileSystem,
	find,
	file,
	folder,
	write,
	append,
	remove,
	isRoot,
	isFile,
	isFolder
)

where

import Path hiding (fromString, parent)
import Data.Maybe (fromJust, isNothing, isJust, listToMaybe)
import Data.List (deleteBy)
import Data.Function (on)


data FileSystem = File {name :: Name, contents :: String, parent :: Maybe FileSystem} 
	| Folder {name :: Name, childern :: [FileSystem], parent :: Maybe FileSystem} 

instance Eq FileSystem where
		a == b = on (==) name a b
	
instance Show FileSystem where
	show (File n c _) =  show n ++ " " ++ show c
	show (Folder n c _) =  show n ++ " " ++ show c
	
isFolder :: FileSystem -> Bool
isFolder (Folder _ _ _) = True
isFolder _ = False

isFile :: FileSystem -> Bool
isFile (File _ _ _) = True
isFile _ = False	
	
root :: FileSystem -> FileSystem
root r @ (Folder _ _ Nothing) = r
root f = root $ fromJust $ parent f

isRoot :: FileSystem -> Bool
isRoot = isNothing . parent

file :: FileSystem -> Name -> String -> Maybe FileSystem
file f n c
	| isFolder f = 
		let new = createFile f n c
		in 
			link f new
	| otherwise = Nothing
	where
		createFile f n c =  File n c $ Just f

folder :: FileSystem -> Name -> Maybe FileSystem
folder f n 
	| isFolder f = 
		let new = createFolder f n
		in
			link f new
	| otherwise = Nothing
	where
		createFolder f n = Folder n [] $ Just f	

link :: FileSystem -> FileSystem -> Maybe FileSystem
link f new  = addChild f new
	where
		addChild (File _ _ _) _ = Nothing
		addChild (Folder n ch p) c = Just $ Folder n (c:ch) p
	
remove :: FileSystem -> Maybe FileSystem
remove f = fmap removal $ parent f
	where
		newChildren = deleteBy (on (==) name) f . childern
		removal e = Folder (name e) (newChildren e) (parent e)
	
write :: FileSystem -> String -> Maybe FileSystem
write (File name _  parent) text = Just $ File name text parent
write _ _ = Nothing
	
append :: FileSystem -> String -> Maybe FileSystem
append f text 
	|isFile f = write f $  contents f ++ text
	| otherwise = Nothing
	
find :: FileSystem -> Path -> Maybe FileSystem
find fs p
	| hasNoContent p && isRelative p = Just fs
	| isRelative p = matching fs $ content p
	| hasNoContent p && isFull p = Just $ root fs
	| isFull p = matching (root fs) $ content p 
	| otherwise = Nothing
	
matching :: FileSystem -> [Name] -> Maybe FileSystem
matching fs (x:xs)
	| xs == [] && nameEquality = Just fs
	| isFolder fs && nameEquality = 
		listToMaybe $ filter (isJust . flip matching xs) $ childern fs 
	| otherwise = Nothing
	where
		nameEquality = name fs == x