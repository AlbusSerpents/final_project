module FileSystem
(
	find,
	file,
	folder,
	write,
	append
)

where

import Path hiding (fromString)
import Data.Maybe (fromJust, isNothing, isJust, listToMaybe)
import Data.List (delete)


data FileSystem = File {name :: Name, contents :: String, parent :: Maybe FileSystem} 
	| Folder {name :: Name, childern :: [FileSystem], parent :: Maybe FileSystem} 
	| Dummy {name :: Name}

instance Eq FileSystem where
		a == b = (name a) == (name b)
	
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
root (File	_ _ parent) = root $ fromJust parent
root (Folder _ _ (Just parent)) = root parent
root r @ (Folder _ _ Nothing) = r

isRoot :: FileSystem -> Bool
isRoot = isNothing . parent

file :: FileSystem -> Name -> String -> Maybe FileSystem
file f n c
	| isFolder f = 
		let new = createFile f n c
		in 
			link f new
	| otherwise = Nothing

folder :: FileSystem -> Name -> Maybe FileSystem
folder f n 
	| isFolder f = 
		let new = createFolder f n
		in
			link f new
	| otherwise = Nothing

link :: FileSystem -> FileSystem -> Maybe FileSystem
link f new  = 	
	if isJust $ addChild f new then
		Just new
	else
		Nothing
	
addChild :: FileSystem -> FileSystem -> Maybe FileSystem
addChild (File _ _ _) _ = Nothing
addChild (Folder n ch p) c = Just $ Folder n (c:ch) p
	
createFile :: FileSystem -> Name -> String -> FileSystem
createFile f n c =  File n c $ Just f
	
createFolder ::FileSystem -> Name -> FileSystem	
createFolder f n = Folder n [] $ Just f	
	
remove :: FileSystem -> Maybe FileSystem
remove f = fmap removal $ parent f
	where
		removalName = Dummy $ name f
		deleted = delete removalName . childern
		removal e = Folder (name e) (deleted e) (parent e)
	
write :: FileSystem -> String -> Maybe FileSystem
write (File name _  parent) text = Just $ File name text parent
write _ _ = Nothing
	
append :: FileSystem -> String -> Maybe FileSystem
append (File name curr parent) text = Just $ File name (curr ++ text) parent
append _ _ = Nothing	
	
find :: FileSystem -> Path -> Maybe FileSystem
find fs p
	| isRelative p = matching fs $ content p
	| isFull p = matching (root fs) $ content p 
	| otherwise = Nothing
	
matching :: FileSystem -> [String] -> Maybe FileSystem
matching fs (x:xs)
	| xs == [] && fs == dummy = Just fs
	| isFolder fs && fs == dummy = 
		listToMaybe $ filter (isJust . flip matching xs) $ childern fs 
	| otherwise = Nothing
	where
		dummy = Dummy x