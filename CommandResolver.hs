module CommandResolver
(
	CommandMacros(..),
	resolveCommand,
	resolveMacro,
	ResolvedCommand
)

where

import Path (Path, fromString)
import Commands hiding (execute, prepare)
import Data.Either (Either(Left), Either(Right))
import Data.List (takeWhile, dropWhile)
import Data.Maybe (listToMaybe)
import Data.Char (toUpper)

import FileSystem

data CommandMacros = PWD | CD |	LS | CAT | RM deriving (Show, Eq, Read)
type ResolvedCommand c = Either c String

resolveMacro :: String -> Maybe CommandMacros
resolveMacro m 
	| m == "pwd" = Just PWD 
	| m == "cd" = Just CD 
	| m == "ls" = Just LS 
	| m == "cat" = Just CAT 
	| m == "rm" = Just RM 
	| otherwise = Nothing

resolveCommand :: (CommandResolver c, Command c) =>
		[String] -> ResolvedCommand c
resolveCommand args = resolve args
	
streamRedirect = ">"

failedResolveError :: String -> String
failedResolveError i = "Error while reading command: " ++ i

class CommandResolver c where
	resolve :: [String] -> Either c String

instance CommandResolver Pwd where
	resolve [] = Left Current
	resolve i = Right $ failedResolveError $ concat i
	
instance CommandResolver Cd where
	resolve (path:[]) = Left $ Change $ fromString path
	resolve i = Right $ failedResolveError $ concat i
	
instance CommandResolver Ls where
	resolve [] = Left $ List Nothing
	resolve (path:[]) = Left $ List $ Just $ fromString path
	resolve i = Right $ failedResolveError $ concat i
	
instance CommandResolver Cat where
	resolve args 
		| streamRedirect `elem` args = 
			Left $ Concat (fromResolve args) (toResolve args)
		| otherwise = Right $ failedResolveError $ concat args

check = (/=) streamRedirect

fromResolve :: [String] -> Maybe [Path]
fromResolve = 
	sequence . 
	map (listToMaybe . (:[]) . fromString) . 
	takeWhile check
	
toResolve :: [String] -> Maybe Path
toResolve = fmap fromString . listToMaybe . tail . dropWhile check

instance CommandResolver Rm where
	resolve paths@(x:xs) = Left $ Remove $ map fromString paths
	resolve i = Right $ failedResolveError $ concat i