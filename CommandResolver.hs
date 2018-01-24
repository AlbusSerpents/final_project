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

data CommandMacros = PWD | CD |	LS | CAT | RM | MKDIR deriving (Show, Eq, Read)
type ResolvedCommand c = Either c String

resolveMacro :: String -> Maybe CommandMacros
resolveMacro m 
	| m == "pwd" = Just PWD 
	| m == "cd" = Just CD 
	| m == "ls" = Just LS 
	| m == "cat" = Just CAT 
	| m == "rm" = Just RM 
	| m == "mkdir" = Just MKDIR 
	| otherwise = Nothing

resolveCommand :: (CommandResolver c, Command c) =>
		[String] -> Path -> ResolvedCommand c 
resolveCommand args = resolve args
	
streamRedirect = ">"

failedResolveError :: String -> String
failedResolveError i = "Error while reading command: " ++ i

class CommandResolver c where
	resolve :: [String] -> Path -> Either c String

instance CommandResolver Pwd where
	resolve [] _ = Left Current
	resolve i _ = Right $ failedResolveError $ concat i
	
instance CommandResolver Cd where
	resolve (path:[]) focus = Left $ Change $ fromString focus path
	resolve i _ = Right $ failedResolveError $ concat i
	
instance CommandResolver Ls where
	resolve [] focus = Left $ List Nothing
	resolve (path:[]) focus = Left $ List $ Just $ fromString focus path
	resolve i _ = Right $ failedResolveError $ concat i
	
instance CommandResolver Cat where
	resolve args focus
		| streamRedirect `elem` args = 
			Left $ Concat (fromResolve focus args) (toResolve focus args)
		| otherwise = Right $ failedResolveError $ concat args

check = (/=) streamRedirect

fromResolve :: Path -> [String] -> Maybe [Path]
fromResolve focus = pullValues . takeWhile check
	where
		pullValues [] = Nothing
		pullValues list = sequence $ map (Just . fromString focus) list
	
toResolve :: Path -> [String] -> Maybe Path
toResolve focus = fmap (fromString focus) . listToMaybe . tail . dropWhile check

instance CommandResolver Rm where
	resolve paths@(x:xs) focus = Left $ Remove $ map (fromString focus) paths
	resolve i _ = Right $ failedResolveError $ concat i
	
instance CommandResolver Mkdir where
	resolve path@(x:[]) focus = Left $ Directory $ fromString focus x
	resolve i _ = Right $ failedResolveError $ concat i