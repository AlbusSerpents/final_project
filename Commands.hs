module Commands
(

)
where

import Path
import FileSystem
import System.IO 
import Data.Maybe (isJust, fromJust, mapMaybe)
import Data.List (intersperse)

data Pwd = Current deriving Show
data Cd = Change {changeArg :: Path} deriving Show
data Ls = List {listArg :: Maybe Path } deriving Show
data Cat = Concat { from :: Maybe [Path], to :: Maybe Path } 
	| ConsoleInput { text :: String, to :: Maybe Path } deriving Show	
data Rm = Remove {removeArgs :: [Path]} deriving Show

class Command c where
	execute :: c -> FileSystem -> Either (FileSystem, String) FileSystem
	prepare :: c -> IO c
	prepare = return
	
processOutput :: Maybe String -> String
processOutput input
	| isJust input = fromJust input
	| otherwise = opperationFailed

opperationFailed = "The requested operation failed. Non- fatal error!"	
	

instance Command Pwd where
	execute curr f = Left (f, show $ fullFileName f)
	
instance Command Cd where
	execute (Change p) f
		| isJust found = Right $ fromJust found
		| otherwise = Left (f, opperationFailed)
		where
			found = find f p
			
instance Command Ls where
	execute (List td) f
		| isJust td = 
			let 
				fs = find f $ fromJust td
				contents = getChildrenNames fs
			in
				Left (f, processOutput contents)
		| otherwise = Left (f, processOutput $ getChildrenNames $ Just f) 
		
		
getChildrenNames :: Maybe FileSystem -> Maybe String
getChildrenNames f = concatChildren <$> f
	where
		concatChildren = concat . intersperse ", " . map name . children