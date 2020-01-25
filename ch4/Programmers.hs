data OperatingSystem = 
       GnuPlusLinux 
     | OpenBSDPlusNevermindJustBSDStill 
     | Mac 
     | Windows
     deriving (Eq, Show)
     
data ProgrammingLanguage =
       Haskell
     | Agda
     | Idris
     | PureScript
     deriving (Eq, Show)
 
data Programmer =
     Programmer { os :: OperatingSystem
                , lang :: ProgrammingLanguage }
     deriving (Eq, Show)
     