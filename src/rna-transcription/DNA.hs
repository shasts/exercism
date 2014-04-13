module DNA (toRNA)  where

toRNA :: String -> String
toRNA input = map mapping input where 
	mapping 'A' = 'U'
	mapping 'C' = 'G'
	mapping 'G' = 'C'
	mapping 'T' = 'A'
	mapping '_' = error "Invalid Mapping"
