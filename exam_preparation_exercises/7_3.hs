--Rewrite functions "map f" and "filter p" with "foldr"
map1 :: (a -> b) -> [a] -> [b]
map1 f = foldr (\fe rv -> [f fe] ++ rv) []

filter1 :: (a -> Bool) -> [a] -> [a]
filter1 p = foldr (\fe rv -> if p fe then fe: rv else rv) []
