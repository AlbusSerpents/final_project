module Path
(
	Path,
	content,
	Name,
	isFull,
	isRelative,
	fromString,
	fromNames,
	hasNoContent,
	isParent,
	parents,
	title,
	root,
	this,
	relative,
	parent
)

where

import System.FilePath (splitPath)

type Name = String

data Path = Full {content :: [Name]} | Relative {content :: [Name]} 
	deriving (Eq)
	
instance Show Path where
	show (Relative c) = show $ concat $ relative:c
	show (Full c) = show $ concat c

this :: Name
this = "."

root :: Name
root = "/"

relative :: Name
relative = this ++ root	

parent :: Name
parent = this ++ this ++ root
	
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
fromNames [] = Relative []
fromNames ns@(x:xs) 
	| root == x = Full xs
	| otherwise = Relative ns