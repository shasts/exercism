module Bob (responseFor) where

import Data.Char

responseFor :: String -> String
responseFor text 
	| isEmptyText text = "Fine. Be that way!"
	| isShout  = "Woah, chill out!" 		
	| last text == '?' = "Sure."
	| otherwise = "Whatever."
	where isShout = any isAlpha text && (all isUpper $ filter isAlpha text)

isEmptyText :: String -> Bool
isEmptyText = all isSpace