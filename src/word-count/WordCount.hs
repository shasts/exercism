module WordCount (wordCount) where

import Data.Map.Strict as M (insertWith, empty, Map)
import Data.Char
import Data.List
import Data.List.Split

wordCount :: String -> Map String Integer
wordCount input = foldl' updateMap M.empty  $ wordsBy isDelimiter $ map toLower input

updateMap :: Map String Integer -> String -> Map String Integer
updateMap values string = M.insertWith (+) string 1 values

isDelimiter :: Char -> Bool
isDelimiter input =  not (isAlphaNum input)
