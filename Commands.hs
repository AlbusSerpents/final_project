module Commands
(

)
where

import Path
import FileSystem
import System.IO

data Pwd = Current deriving Show
data Cd = Change {changeArg :: Path} deriving Show
data Ls = List {listArg :: Maybe Path } deriving Show
data Cat = Concat { from :: Maybe [Path], to :: Maybe Path } deriving Show	
data Rm = Remove {removeArgs :: [Path]} deriving Show

class Command c where
	execute :: c -> FileSystem -> Either (FileSystem, String) FileSystem
	prepare :: c -> IO c
	prepare = return