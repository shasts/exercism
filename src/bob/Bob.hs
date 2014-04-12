module Bob
(responseFor) where

import Data.Text as T
import Data.Char

responseFor :: [Char] -> [Char]
responseFor xs = responseForInternal $ T.pack xs

isEmptyText :: Text -> Bool
isEmptyText text = T.length (T.strip text) == 0

responseForInternal :: Text -> [Char]
responseForInternal text 
	|((T.any (isLower) text) == False) && (T.any(isUpper) text)  = "Woah, chill out!"
	| isEmptyText text = "Fine. Be that way!"
	| T.last text == '?' = "Sure."
	| otherwise = "Whatever."