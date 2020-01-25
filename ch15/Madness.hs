import Data.Monoid

type Verb = String
type Adjective = String
type Adverb = String
type Noun = String
type Exclamation = String

madlibbin' :: Exclamation 
              -> Adverb
              -> Noun
              -> Adjective
              -> String
           
madlibbin' exl adv noun adj =
  exl <> "!  he said " <>
  adv <> " as he jumped into his " <>
  noun <> " and drove off with his " <>
  adj <> " wife."
  
madlibbinBetter' :: Exclamation 
                    -> Adverb
                    -> Noun
                    -> Adjective
                    -> String
                    
madlibbinBetter' exl adv noun adj =
  mconcat [exl, "! he said ",
           adv, " as he jumped into his ",
           noun," and drove off with his ",
           adj, " wife."]