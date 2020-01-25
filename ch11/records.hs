data Person = 
  Person { name :: String , age :: Int }
     deriving (Eq, Show)
     
jm = Person "Julie" 100
ca = Person "Chris" 16