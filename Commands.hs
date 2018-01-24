module Commands
(
	Pwd(..),
	Cd(..),
	Ls(..),
	Cat(Concat),
	Rm(..),
	Mkdir(..),
	Response,
	Command(execute, prepare)	
)
where

import Path
import FileSystem
import Data.Maybe (isJust, fromJust, mapMaybe, fromMaybe)
import Data.List (intersperse)

data Pwd = Current deriving Show
data Cd = Change {changeArg :: Path} deriving Show
data Ls = List {listArg :: Maybe Path } deriving Show
data Cat = Concat { from :: Maybe [Path], to :: Maybe Path } 
	| ConsoleInput { input :: String, to :: Maybe Path } deriving Show	
data Rm = Remove {removeArgs :: [Path]} deriving Show
data Mkdir = Directory {createArg :: Path} deriving Show

type Response = Either (FileSystem Bool, String) (FileSystem Bool)

class Command c where
	execute :: c -> FileSystem a -> Response
	prepare :: c -> IO c
	prepare = return
	
createResponse :: FileSystem Bool -> Response
createResponse r 
	| result r = Right r
	| otherwise = Left (r, operationFailed)

operationFailed = "The requested operation failed. Non- fatal error!"	
	
instance Command Pwd where
	execute curr r = Left (changeResult r True, show $ focus r)
	
instance Command Cd where
	execute (Change path) r = createResponse $ changeFocus r path
			
instance Command Ls where
	execute (List path) r 
		| isJust path = Left (changeResult r True, processResponse $ getFolderContents r $ listPath path)
		| otherwise = Left (changeResult r True, processResponse $ getFolderContents r $ focus r)
		where
			listPath = fromJust

processResponse :: FileSystem (Bool, Maybe String) -> String
processResponse r
	| isJust $ snd $ result r = fromJust $ snd $ result r
	| otherwise = operationFailed
		
instance Command Cat where
	execute c r = cat c r
	prepare (Concat Nothing d) = do
		_lines <- readInput
		let textData = unlines _lines
			in do
				return (ConsoleInput textData d)
	prepare c = return (c)

cat :: Cat -> FileSystem a -> Response
cat (Concat (Just src) (Just dst)) r = createAndWrite r dst $ readFromFiles r src
cat (Concat (Just src) Nothing) r = Left (changeResult r True, readFromFiles r src)
cat (ConsoleInput input Nothing) r = Left (changeResult r True, input)
cat (ConsoleInput input (Just dst)) r = createAndWrite r dst input
	
readFromFiles :: FileSystem a -> [Path] -> String
readFromFiles r = foldl (\b -> \a -> readConcat b $ result a) "" . map (readFromFile r)
	where
		readConcat base (_, Nothing) = base
		readConcat base (_, Just s) = s ++ base
	
readInput :: IO [String]
readInput = do
	input <- getLine
	let 
		processed = handleInput input
		in do
		if processed == [] then
			return processed
		else do
			next <- readInput
			return (processed++next)
	where
		handleInput "." = []
		handleInput s = [s]
	
createAndWrite :: FileSystem a -> Path -> String -> Response
createAndWrite r p text = 
	createResponse $
	if result $ exists r p then
		writeToFile r p text
	else
		file r p (title p) text
			
instance Command Rm where
	execute (Remove paths) r = remove paths r

remove :: [Path] -> FileSystem a -> Response
remove [] r = createResponse $ changeResult r True
remove (p:ps) r = remove ps $ removeElement r p 

instance Command Mkdir where
	execute (Directory path) r = createResponse $ folder r dirPath dirName 
		where
			dirName = title path
			dirPath = path