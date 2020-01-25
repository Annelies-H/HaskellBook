{-# LANGUAGE FlexibleInstances #-}

module ChapterExercises where

-- rearrange the arguments so the functor instance works

data Sum b a = First a | Second b

instance Functor (Sum e) where
  fmap f (First a) = First (f a)
  fmap _ (Second b) = Second b
  
data Company a c b = DeepBlue a c | Something b

instance Functor (Company e e') where
  fmap f (Something b) = Something (f b)
  fmap _ (DeepBlue a c) = DeepBlue a c
  
data More b a = L a b a | R b a b deriving (Eq, Show)

instance Functor (More x) where
  fmap f (L  a b a') = L (f a) b (f a')
  fmap f (R b a b') = R b (f a) b'
  
-- write functor instances for the following datatypes

data Quant a b = Finance | Desk a | Bloor b deriving (Eq, Show)

instance Functor (Quant a) where
  fmap f (Bloor b) = Bloor (f b)
  fmap _ Finance = Finance
  fmap _ (Desk a) = Desk a
  
--
  
data K a b = K a deriving (Eq, Show)



instance Functor (K a) where
  fmap _ (K a) = K a
  
--  
data Flip f a b = Flip (f b a)
-- newtype Flip (f :: * -> * -> *) (a :: *) (b :: *) = Flip (f b a) deriving (Eq, Show)

-- In onderstaand geval heeft K de plek in genomen van de f, K heeft namelijk twee argumenten
-- Op het type niveau heb je geen functies alleen type constructors, zoals K
instance Functor (Flip K a) where
  fmap f (Flip (K b)) = Flip (K (f b))
  
--
  
data EvilGoateeConst a b = GoatyConst b deriving (Eq, Show)

instance Functor (EvilGoateeConst a) where
  fmap f (GoatyConst b) = GoatyConst (f b)
  
--

data LiftItOut f a = LiftItOut (f a) deriving (Eq, Show)

-- the f in LiftItOut is een type variable met kind * -> *
-- f in LiftItOut is dus geen functie!!!!!!!
instance Functor f => Functor (LiftItOut f) where
  fmap f (LiftItOut fa) = LiftItOut (fmap f fa)

--

data Parappa f g a = DaWrappa (f a) (g a) deriving (Eq, Show)

instance (Functor f, Functor g) => Functor (Parappa f g) where
  fmap f (DaWrappa fa ga) = DaWrappa (fmap f fa) (fmap f ga)
  
--

data IgnoreOne f g a b = IgnoreSomething (f a) (g b) deriving (Eq, Show)

instance Functor g => Functor (IgnoreOne f g a) where
  fmap f (IgnoreSomething fa gb) = IgnoreSomething fa (fmap f gb)
  
--

data Notorious g o a t = Notorious (g o) (g a) (g t) deriving (Eq, Show)

instance Functor g => Functor (Notorious g o a) where
  fmap f (Notorious go ga gt) = Notorious go ga (fmap f gt)
  
--

data List a = Nil | Cons a (List a) deriving (Eq, Show)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons a x) = Cons (f a) (fmap f x)
  
--

data GoatLord a = 
    NoGoat 
  | OneGoat a 
  | MoreGoats (GoatLord a) (GoatLord a) (GoatLord a)
  deriving (Eq, Show)
  
instance Functor GoatLord where
  fmap _  NoGoat           = NoGoat
  fmap f (OneGoat a)       = OneGoat (f a)
  fmap f (MoreGoats x y z) = 
    MoreGoats (fmap f x) (fmap f y) (fmap f z)
    
--

data TalkToMe a = Halt | Print String a | Read (String -> a)

instance Functor TalkToMe where
  fmap _ Halt = Halt
  fmap f (Print string a) = Print string (f a)
  fmap f (Read g) = Read (f.g)

