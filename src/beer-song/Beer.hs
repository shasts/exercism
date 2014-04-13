module Beer (sing, verse) where

sing :: Int -> Int -> String
sing end start = foldl (\song count -> song ++ verse count ++ "\n") [] [end, (end-1)..start]

verse :: Int -> String
verse count = sayCurrent count ++"\n" ++ action count ++ sayNext count ++ "\n"

sayCurrent :: Int -> String
sayCurrent count = current count True ++ "of beer on the wall, " ++ current count False ++ "of beer."

action :: Int -> String
action 0 = "Go to the store and buy some more, "
action count = "Take "++ howMany count ++ " down and pass it around, "

sayNext :: Int -> String
sayNext 0 = "99 bottles of beer on the wall."
sayNext count = current (count -1) False ++ "of beer on the wall."

howMany :: Int -> String
howMany 1 = "it"
howMany count = "one"

current :: Int -> Bool -> String
current 0 True = "No more bottles "
current 0 False = "no more bottles "
current 1 _ = "1 bottle "
current count _ = show count ++ " bottles "