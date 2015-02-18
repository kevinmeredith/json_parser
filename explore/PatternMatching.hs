--f :: String -> Bool
--f ('f':xs:'b') = True
--f _            = False

g :: String -> Bool
g ('\\':'"':_) = True
g _ 		   = False

f :: String -> [Char]
f ('"':xs) = xs
f _        = []
