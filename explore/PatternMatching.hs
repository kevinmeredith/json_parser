--f :: String -> Bool
--f ('f':xs:'b') = True
--f _            = False

g :: String -> Bool
g ('\\':'"':_) = True
g _ 		   = False