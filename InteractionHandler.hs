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

processCommand :: FileSystem Bool -> Maybe CommandMacros -> [String] -> IO Response
processCommand r jMacro arr
	| isNothing jMacro = return $ Left (r, "Invalid command!")
	| cmd == PWD = executeCommand r $ (resolveCommand arr path :: ResolvedCommand Pwd)
	| cmd == CD = executeCommand r $ (resolveCommand arr path :: ResolvedCommand Cd)
	| cmd == LS = executeCommand r $ (resolveCommand arr path :: ResolvedCommand Ls)
	| cmd == CAT = executeCommand r $ (resolveCommand arr path :: ResolvedCommand Cat)
	| cmd == RM = executeCommand r $ (resolveCommand arr path :: ResolvedCommand Rm)
	| otherwise = return $ Left (r, "Unsupported command!")
	where
		cmd = fromJust jMacro
		path = focus r

test :: (Command c) => FileSystem Bool -> ResolvedCommand c -> IO Response
test r (Left c) = do
	newC <- prepare c
	return $ execute newC r
	
executeCommand :: (Command c) => FileSystem Bool -> ResolvedCommand c -> IO Response
executeCommand r (Right err) = return $ Left (r, err)
executeCommand r (Left c) = do
	newC <- prepare c
	return $ execute newC r
		
processResponse :: Response -> IO (FileSystem Bool)
processResponse (Right r) = do return r
processResponse (Left (r, str)) = do
	putStrLn str
	return r
		
processInput :: FileSystem Bool -> String -> [String] -> IO (FileSystem Bool)
processInput r m args = do
	resp <- processCommand r macro args
	newR <- processResponse resp
	return newR
	where
		macro = resolveMacro m
	
interactiveMode :: FileSystem Bool -> IO ()
interactiveMode r = do
	input <- getLine
	case words input of 
		[] -> interactiveMode r
		(":q":[]) -> return ()
		(macro:args) -> do
			newFs <- processInput r macro args
			interactiveMode newFs
