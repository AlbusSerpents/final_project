module Path
(
	Path,
	content,
	Name,
	isFull,
	isRelative,
	fromString
)

where

import System.FilePath (splitPath)

type Name = String

data Path = Full {content :: [Name]} | Relative {content :: [Name]} 
	deriving (Show, Eq)
	
root = "/"	
	
isFull :: Path -> Bool
isFull (Full _) = True
isFull _ = False

isRelative :: Path -> Bool
isRelative (Relative _) = True
isRelative _ = False

fromString :: FilePath -> Path
fromString fp 
	| head split == root = Full split
	| otherwise = Relative split
	where
		split = splitPath fp

	
