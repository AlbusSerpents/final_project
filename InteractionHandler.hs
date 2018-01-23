module InteractionHandler
(
	interactiveMode
)

where

import Data.Maybe (fromJust, isNothing, Maybe)
import Commands
import FileSystem
import CommandResolver
import Path

import System.IO

processCommand :: FileSystem -> Maybe CommandMacros -> [String] -> IO Response
processCommand f jMacro arr
	| isNothing jMacro = return $ Left (f, "Invalid command!")
	| cmd == PWD = executeCommand f $ (resolveCommand arr :: ResolvedCommand Pwd)
	| cmd == CD = executeCommand f $ (resolveCommand arr :: ResolvedCommand Cd)
	| cmd == LS = executeCommand f $ (resolveCommand arr :: ResolvedCommand Ls)
	| cmd == CAT = executeCommand f $ (resolveCommand arr :: ResolvedCommand Cat)
	| cmd == RM = executeCommand f $ (resolveCommand arr :: ResolvedCommand Rm)
	| otherwise = return $ Left (f, "Unsupported command!")
	where
		cmd = fromJust jMacro

test :: (Command c) => FileSystem -> ResolvedCommand c -> IO Response
test fs (Left c) = do
	newC <- prepare c
	return $ execute newC fs
	
executeCommand :: (Command c) => FileSystem -> ResolvedCommand c -> IO Response
executeCommand fs (Right err) = return $ Left (fs, err)
executeCommand fs (Left c) = do
	newC <- prepare c
	return $ execute newC fs
		
processResponse :: Response -> IO FileSystem
processResponse (Right fs) = do return (fs)
processResponse (Left (fs, str)) = do
	putStrLn str
	return (fs)
		
processInput :: FileSystem -> String -> [String] -> IO FileSystem
processInput fs m args = do
	resp <- processCommand fs macro args
	newFs <- processResponse resp
	return newFs
	where
		macro = resolveMacro m
	
interactiveMode :: FileSystem -> IO ()
interactiveMode fs = do
	input <- getLine
	case words input of 
		[] -> interactiveMode fs
		(":q":[]) -> return ()
		(macro:args) -> do
			newFs <- processInput fs macro args
			interactiveMode newFs
	where
		quitMacro = ":q"