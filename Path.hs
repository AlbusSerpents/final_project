module Path
(
	Path,
	Name,
	fromString,
	parents,
	title,
	root,
	this,
	parent,
	parentOnly,
	rootPath,
	unroot,
	content
)

where

import System.FilePath (splitPath)
import Data.List (delete, intersperse)

type Name = String

data Path = Absolute {content :: [Name]} | Relative {content :: [Name], base :: Path} 
	deriving (Eq)
	
instance Show Path where
	show (Relative c _) = show $ concat $ relative:c
	show (Absolute c) = show $ concat $ (head c):(intersperse root $ tail c)

this :: Name
this = "."

root :: Name
root = "/"

relative :: Name
relative = this ++ root	

parentOnly :: Name
parentOnly = this ++ this

parent :: Name
parent = this ++ this ++ root

rootPath :: Path
rootPath = Absolute [root]
	
title :: Path -> Name
title = last . content

parents :: Path -> Path
parents = Absolute . init . content

unroot :: Path -> [Name]
unroot (Absolute []) = []
unroot (Absolute (n:ns)) = ns
	
toAbsolute :: Path -> Path
toAbsolute a@(Absolute c) = a
toAbsolute (Relative c p) = Absolute $ absoluteName ++ c 
	where
		absoluteName = content $ toAbsolute p
	
fromString :: Path -> FilePath -> Path
fromString p fp 
	| fp == parentOnly = toAbsolute $ Relative [] $ parents p
	| head split == root = Absolute $ root:(map (delete '/') $ tail split)
	| head split == parent = toAbsolute $ Relative (tail split) $ parents p
	| otherwise = toAbsolute $ Relative split p
	where
		split = splitPath fp