module Path
(
	Path,
	content,
	Name,
	isFull,
	isRelative,
	fromString,
	parent,
	hasNoContent
)

where

import System.FilePath (splitPath)

type Name = String

data Path = Full {content :: [Name]} | Relative {content :: [Name]} 
	deriving (Eq)
	
instance Show Path where
	show (Relative c) = show $ concat $ relative:c
	show (Full c) = show $ concat $ root:c

relative :: Name
relative = "./"	
root :: Name
root = "/"
parent :: Name
parent = "../"
	
isFull :: Path -> Bool
isFull (Full _) = True
isFull _ = False

isRelative :: Path -> Bool
isRelative (Relative _) = True
isRelative _ = False

fromString :: FilePath -> Path
fromString fp 
	| head split == root = Full $ tail split
	| otherwise = Relative split
	where
		split = splitPath fp

hasNoContent :: Path -> Bool
hasNoContent p = content p == []