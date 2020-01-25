fmap :: Functor f => (a -> b) -> (f a -> f b)
(fmap . fmap) replaceWithP == fmap (fmap replaceWithP) 

replaceWithP :: a -> Char
replaceWithP = const 'p'

lms :: [Maybe String]
lms = [Just "Ave", Nothing, "Just "woohoo"

fmap `asAppliedTo` replaceWithP :: (a -> Char) -> (f a -> f Char)
fmap replaceWithP :: Functor f => f a -> f Char
fmap replaceWithP lms :: [] (Maybe String) -> [] Char
fmap replaceWithP lms = "ppp"

fmap `asAppliedTo` fmap replaceWithP :: (Functor f, Functor g) => (f a -> f Char) -> g (f a) -> g (f Char)
fmap (fmap replaceWithP) :: (Functor f, Functor g) => g (f a) -> g (f Char)
fmap (fmap replaceWithP) lms::  [] (Maybe a) -> [] (Maybe Char)
(fmap . fmap) replaceWithP lms = [Just 'p', Nothing, Just 'p'

fmap `asAppliedTo` fmap (fmap replaceWithP) :: (Functor f, Functor g, Functor h) 
                                                => (g (f a) -> g (f Char)) -> h (g (f a) -> h (g (f Char)
fmap ( fmap (fmap replaceWithP)) :: (Functor f, Functor g, Functor h) => h (g (f a) -> h (g (f Char)   
fmap (fmap (fmap replaceWithP)) lms :: [] (Maybe ([] Char))
(fmap . fmap . fmap) replaceWithP lms = [Just "ppp", Nothing, Just "pppppp"]
                                 