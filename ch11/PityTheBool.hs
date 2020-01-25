import Data.Int

data BigSmall = Big Bool | Small Bool deriving (Eq, Show)

-- Big Small has two data constructors with each two Bool constructors, which have a cardinallity of two
-- Big Bool + Small Bool = 2 + 2 = 4

data NumberOrBool = Numba Int8 | BoolyBool Bool deriving (Eq, Show)

-- Numba Int8 | BoolyBool Bool = 256 + 2 = 258