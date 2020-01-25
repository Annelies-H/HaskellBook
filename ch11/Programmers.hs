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
 
 
allOperatingSystems :: [OperatingSystem]
allOperatingSystems = [GnuPlusLinux, OpenBSDPlusNevermindJustBSDStill, Mac, Windows]

allLanguages :: [ProgrammingLanguage]
allLanguages = [Haskell, Agda, Idris, PureScript]

allProgrammers :: [Programmer]
allProgrammers = twotup allOperatingSystems allLanguages
      where
        twotup _ [] = []
        twotup xs (y:ys) = tupx xs y ++ twotup xs ys
          where
            tupx [] _ = []
            tupx (x:xs) y =  Programmer x y  : tupx xs y

allProgrammers' :: [Programmer]
allProgrammers' = [Programmer {os = o, lang = l} | o <- allOperatingSystems, l <- allLanguages]
