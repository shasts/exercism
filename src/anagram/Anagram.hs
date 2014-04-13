module Anagram (anagramsFor) where

import Data.List
import Data.Char

anagramsFor :: String -> [String] -> [String]
anagramsFor _ [] = []
anagramsFor seed (x:xs)
	| seed == x = anagramsFor seed xs
	| caseIgnoredSeed == (sort $ map toLower x) = x:anagramsFor seed xs
	| otherwise = anagramsFor seed xs
	where caseIgnoredSeed = sort $ map toLower seed