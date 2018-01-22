module CommandResolver
(

)

where

import Path
import Commands
import Data.Either

--resolve :: (Command c) => [String] -> Either c String
--resolve ("pwd":[]) = Left Current
--resolve ("cd":ps:[]) = Left $ Change $ fromString ps
--resolve ("ls":ps) = undefined --LS
--resolve ("ls":[]) = undefined --LS
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
