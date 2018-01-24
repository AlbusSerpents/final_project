module Path
(
	Path,
	Name,
	fromString,
	parents,
	title,
	root,
	parent,
	rootPath,
	unroot,
	toAbsolute
)

where

import System.FilePath (splitPath)

type Name = String

data Path = Absolute {content :: [Name]} | Relative {content :: [Name], base :: Path} 
	deriving (Eq)
	
instance Show Path where
	show (Relative c _) = show $ concat $ relative:c
	show (Absolute c) = show $ concat $ c

this :: Name
this = "."

root :: Name
root = "/"

relative :: Name
relative = this ++ root	

parent :: Name
parent = this ++ this ++ root

rootPath :: Path
rootPath = Absolute [root]
	
title :: Path -> Name
title = last . content

parents :: Path -> Path
parents = Absolute . init . content

unroot :: Path -> [Name]
unroot (Absolute (n:ns)) = ns
	
toAbsolute :: Path -> Path
toAbsolute a@(Absolute c) = a
toAbsolute (Relative c p) = Absolute $ absoluteName ++ c 
	where
		absoluteName = content $ toAbsolute p
	
fromString :: Path -> FilePath -> Path
fromString p fp 
	| head split == root = Absolute $ tail split
	| otherwise = Relative split p
	where
		split = splitPath fp