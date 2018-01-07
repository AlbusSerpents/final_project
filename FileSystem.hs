module FileSystem
(

)

where

import Data.Maybe

data FileSystem = File {contents :: String, name :: String, parent :: Maybe FileSystem} 
	| Folder {childern :: [FileSystem], name :: String, parent :: Maybe FileSystem}

data Path = Full {content :: String} | Relative {content :: String} 
	deriving Show

root :: FileSystem -> FileSystem
root (File	_ _ parent) = root $ fromJust parent
root (Folder _ _ (Just parent)) = root parent
root r @ (Folder _ _ Nothing) = r

findElement :: FileSystem -> Path -> FileSystem
findElement _ _ = undefined