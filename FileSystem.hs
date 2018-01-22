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
	isFolder,
	fullFileName,
	name,
	children,
	contents
)

where

import qualified Path as P
import Data.Maybe (fromJust, isNothing, isJust, listToMaybe)
import Data.List (deleteBy)
import Data.Function (on)


data FileSystem = File {name :: P.Name, contents :: String, parent :: Maybe FileSystem} 
	| Folder {name :: P.Name, children :: [FileSystem], parent :: Maybe FileSystem} 

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

file :: FileSystem -> P.Name -> String -> Maybe FileSystem
file f n c
	| isFolder f = 
		let new = createFile f n c
		in 
			link f new
	| otherwise = Nothing
	where
		createFile f n c =  File n c $ Just f

folder :: FileSystem -> P.Name -> Maybe FileSystem
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
		newChildren = deleteBy (on (==) name) f . children
		removal e = Folder (name e) (newChildren e) (parent e)
	
write :: FileSystem -> String -> Maybe FileSystem
write (File name _  parent) text = Just $ File name text parent
write _ _ = Nothing
	
append :: FileSystem -> String -> Maybe FileSystem
append f text 
	|isFile f = write f $  contents f ++ text
	| otherwise = Nothing

fullFileName :: FileSystem -> P.Path
fullFileName f = P.fromNames $ P.root:(reverse $ names $ Just f)
	where	
		names Nothing = []	
		names (Just e) = (name e):(names $ parent e)	
	
find :: FileSystem -> P.Path -> Maybe FileSystem
find fs p
	| P.hasNoContent p && P.isRelative p = Just fs
	| P.isRelative p = matching fs $ P.content p
	| P.hasNoContent p && P.isFull p = Just $ root fs
	| P.isFull p = matching (root fs) $ P.content p 
	| otherwise = Nothing
	
matching :: FileSystem -> [P.Name] -> Maybe FileSystem
matching fs (x:xs)
	| P.isParent x && xs == [] = parent fs
	| P.isParent x = parent fs >>= (\f -> matching f xs) 
	| xs == [] && nameEquality = Just fs
	| isFolder fs && nameEquality = 
		listToMaybe $ filter (isJust . flip matching xs) $ children fs 
	| otherwise = Nothing
	where
		nameEquality = name fs == x