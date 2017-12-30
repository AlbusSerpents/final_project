module Commands
(
	Command,
	Path
)

where

import System.FilePath
import System.Directory

import System.Process
clear = system "clear"

data Path = Full {content :: String} | Relative {content :: String} 
	deriving Show

data Command = 
	Current | 
	Change {arg :: Path}| 
	List {argument :: Maybe Path} | 
	Concat {files :: [Path], output :: Maybe Path} |
	Write {text :: String, output :: Maybe Path} |
	Remove {args :: [Path]} deriving Show
	
data CommandHeaders = PWD | CD | LS |CAT | RM deriving (Show, Read)

resolvePath :: String -> Path
resolvePath f @ ('/':rest) = Full f
resolvePath p = Relative p 


main = do
	interact func

func "." = ""
func s = s
