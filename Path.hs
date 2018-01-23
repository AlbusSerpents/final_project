module Path
(
	Path,
	content,
	Name,
	isFull,
	isRelative,
	fromString,
	parent,
	hasNoContent,
	fromNames,
	root,
	relative,
	isParent,
	parents,
	title
)

where

import System.FilePath (splitPath)

type Name = String

data Path = Full {content :: [Name]} | Relative {content :: [Name]} 
	deriving (Eq)
	
instance Show Path where
	show (Relative c) = show $ concat $ relative:c
	show (Full c) = show $ concat c

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

hasNoContent :: Path -> Bool
hasNoContent p = content p == []

isParent :: Name -> Bool
isParent n = n == parent

title :: Path -> Name
title = last . content

parents :: Path -> [Name]
parents = init . content

fromString :: FilePath -> Path
fromString fp 
	| head split == root = Full $ tail split
	| otherwise = Relative split
	where
		split = splitPath fp

fromNames :: [Name] -> Path
fromNames ns@(x:xs) 
	| root == x = Full ns
	| otherwise = Relative ns