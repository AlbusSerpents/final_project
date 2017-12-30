module Commands
(
	Pwd(..),
	Cd(..),
	Ls(..),
	Cat(..),
	Rm(..),
	Path
)

where

import System.FilePath
import System.Directory

import System.Process
clear = system "clear"

data Path = Full {content :: String} | Relative {content :: String} 
	deriving Show

data Pwd = Current deriving Show
data Cd = Cd {arg :: Path} deriving Show
data Ls = Other {argument :: Maybe Path} deriving Show
data Cat = Concat {files :: [Path], output :: Maybe Path}
			| Write {text :: String, output :: Maybe Path} deriving Show
data Rm = Remove {args :: [Path]} deriving Show

instance Read Path where
read f@ ('/':rest) = Full f 
read p = Relative p 


main = do
	interact func

func "." = ""
func s = s
