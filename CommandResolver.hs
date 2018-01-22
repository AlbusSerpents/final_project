module CommandResolver
(

)

where

import Path
import Commands
import Data.Either
import Data.List (takeWhile, dropWhile)
import Data.Maybe

--resolve ("cat":[]) = undefined --CAT
--resolve ("rm":[]) = undefined --RM
--resolve _ = undefined

streamRedirect = ">"

failedResolveError :: String -> String
failedResolveError i = "Error while reading command " ++ i

class CommandResolver c where
	resolve :: [String] -> Either c String

instance CommandResolver Pwd where
	resolve ("pwd":[]) = Left Current
	resolve i = Right $ failedResolveError $ concat i
	
instance CommandResolver Cd where
	resolve ("cd":paths:[]) = Left $ Change $ fromString paths
	resolve i = Right $ failedResolveError $ concat i
	
instance CommandResolver Ls where
	resolve ("ls":[]) = Left $ List Nothing
	resolve ("ls":path:[]) = Left $ List $ Just $ fromString path
	resolve i = Right $ failedResolveError $ concat i
	
instance CommandResolver Cat where
	resolve ("cat":args) = Left $ Concat (fromResolve args) (toResolve args)
	resolve i = Right $ failedResolveError $ concat i

check = (/=) streamRedirect

fromResolve :: [String] -> Maybe [Path]
fromResolve = 
	sequence . 
	map (listToMaybe . (:[]) . fromString) . 
	takeWhile check
	
toResolve :: [String] -> Maybe Path
toResolve = fmap fromString . listToMaybe . tail . dropWhile check

instance CommandResolver Rm where
	resolve ("rm":args) = Left $ Remove $ map fromString args
	resolve i = Right $ failedResolveError $ concat i