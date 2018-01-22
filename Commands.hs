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
	| ConsoleInput { input :: String, to :: Maybe Path } deriving Show	
data Rm = Remove {removeArgs :: [Path]} deriving Show

type Response = Either (FileSystem, String) FileSystem

isConcat :: Cat -> Bool
isConcat (Concat _ _) = True
isConcat _ = False

isConsole :: Cat -> Bool
isConsole (ConsoleInput _ _) = True
isConsole _ = False

class Command c where
	execute :: c -> FileSystem -> Response
	prepare :: c -> IO c
	prepare = return
	
processOutput :: Maybe String -> String
processOutput input
	| isJust input = fromJust input
	| otherwise = operationFailed
	
createResponse :: FileSystem -> Maybe FileSystem -> Response
createResponse _ (Just success) = Right success
createResponse baseContext Nothing = Left (baseContext, operationFailed)

operationFailed = "The requested operation failed. Non- fatal error!"	
	

instance Command Pwd where
	execute curr f = Left (f, show $ fullFileName f)
	
instance Command Cd where
	execute (Change p) f
		| isJust found = Right $ fromJust found
		| otherwise = Left (f, operationFailed)
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
		
instance Command Cat where
	execute c fContext 
		| isConcat c && hasSources && hasDestination = 
			handleCatDestination fContext dest text
		| isConcat c && hasSources && not hasDestination = Left (fContext, text)
		| isConsole c && not hasDestination = Left (fContext, text)
		| isConsole c && hasDestination = 
			handleCatDestination fContext dest $ input c
		| otherwise = Left (fContext, operationFailed)
		where
			hasSources = isJust $ from c
			hasDestination = isJust $ to c
			src = fromJust $ from c
			dest = fromJust $ to c
			text = getFileContents fContext src

getFileContents :: FileSystem -> [Path] -> String	
getFileContents f =  concat . map contents . filter isFile . mapMaybe (find f)

handleCatDestination :: FileSystem -> Path -> String -> Response
handleCatDestination fs dest text = 
	createResponse fs $
	Just fs >>= 
	(\f -> writeToFile fs dest text) >>=
	(\f -> find f $ fullFileName fs)

writeToFile :: FileSystem -> Path -> String -> Maybe FileSystem
writeToFile fs p text = 
	let
		newFile = find fs p
		fileName = title p
		parentPath = fromNames $ parents p
	in
		if isJust newFile then
			write (fromJust newFile) text
		else
			Just fs >>= 
			(\f -> find f parentPath) >>=
			(\f -> file f fileName text)