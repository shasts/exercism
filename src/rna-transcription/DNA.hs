module DNA (toRNA)  where

import Data.Maybe

toRNA :: String -> String
toRNA = map dnaToRna where 
	dnaToRna 'A' = 'U'
	dnaToRna 'C' = 'G'
	dnaToRna 'G' = 'C'
	dnaToRna 'T' = 'A'
	dnaToRna dna = error $ "Invalid nucleotide: " ++ show dna
