module WordCount (wordCount) where

import Data.Map as M (insertLookupWithKey, empty, Map)
import Data.Maybe
import Data.Char

wordCount :: String -> Map String Integer
wordCount [] = M.empty
wordCount input = foldl (\counter word -> snd $ updateMap counter word) M.empty  $ words $ preprocess input

updateMap :: Map String Integer -> String -> (Maybe Integer, Map String Integer)
updateMap values string = M.insertLookupWithKey (\ _ _ oldvalue -> oldvalue+1) string 1 values

preprocess :: String -> String
preprocess [] = []
preprocess (x:xs)
    | x == '_' = ' ': preprocess xs
    | x == ',' = ' ': preprocess xs
    | x == ' ' = ' ': preprocess xs
    | isAlphaNum x = toLower x:preprocess xs
    | otherwise = preprocess xs