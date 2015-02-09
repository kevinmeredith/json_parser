data Foo a = Bar Int | Baz Bool deriving Show

-- see this question http://stackoverflow.com/questions/28395296/attempting-to-implement-functor-on-phantom-type

instance Functor Foo where
	fmap f (Bar x) = Bar (f x)
	fmap f (Baz x) = Baz (f x)