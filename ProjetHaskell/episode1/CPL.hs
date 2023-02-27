module CPL
  ( Formula (..),
    World,
    genAllWorlds,
    genChaqueWorlds,
    sat,
    extrait,
    extraitAll,
    deleteDoublons,
  )
where

data Formula
  = Var String
  | T
  | F
  | And (Formula) (Formula)
  | Or (Formula) (Formula)
  | Not (Formula)
  | Imp (Formula) (Formula)
  | Eqv (Formula) (Formula)
  deriving (Show, Eq)

type World = [String]

genAllWorlds :: World -> [World]
genAllWorlds [] = [[]]
genAllWorlds [p] = [[], [p]]
genAllWorlds (x : xs) = genChaqueWorlds x (genAllWorlds xs)

{-
genChaqueWorlds représente ma fonction intermédiaire, elle permet de concactener chaque chaine
                de caractère avec l'ensemble des chaines restantes de la liste. Ce qui nous permet d'obtenir l'ensembles des mondes possibles

-}
genChaqueWorlds :: String -> [World] -> [World]
genChaqueWorlds _ [] = []
genChaqueWorlds v (x : xs) = x : (v : x) : genChaqueWorlds v xs

sat :: World -> Formula -> Bool
sat _ T = True
sat _ F = False
sat [] _ = False
sat (x : xs) (Var chaine)
  | x == chaine = True
  | otherwise = sat xs (Var chaine)

  
sat (x : xs) (Not phi)
  | sat (x : xs) phi == False = True
  | otherwise = False
{-
  | contient x (Var chaine) = True
  | otherwise = sat xs (Var chaine)
   -}

sat (x : xs) (And phi psi) = sat (x : xs) (phi) && sat (x : xs) (psi)
sat (x : xs) (Or phi psi) = sat (x : xs) (phi) || sat (x : xs) (psi)
sat (x : xs) (Imp phi psi)
  | sat (x : xs) (And phi psi) == True = True -- phi Vrai psi Vrai => Vrai
  | (sat (x : xs) phi == True) && (sat (x : xs) psi == False) = False -- phi Vrai psi Faux => Faux
  | (sat (x : xs) phi == False) && (sat (x : xs) psi == True) = True -- phi Faux psi Vrai => Vrai
  | otherwise = True -- phi Faux psi Faux => Vrai
sat (x : xs) (Eqv phi psi)
  | sat (x : xs) (And phi psi) == True = True -- phi Vrai psi Vrai => Vrai
  | (sat (x : xs) phi == False) && (sat (x : xs) psi == False) = True -- phi Faux psi Faux => Vrai
  | otherwise = False -- phi Faux psi Faux => Vrai

extraitAll :: Formula -> World
extraitAll (Var string) = [string]
extraitAll T = []
extraitAll F = []
extraitAll (And phi psi) = extrait phi ++ extrait psi
extraitAll (Or phi psi) = extrait phi ++ extrait psi
extraitAll (Imp phi psi) = extrait phi ++ extrait psi
extraitAll (Eqv phi psi) = extrait phi ++ extrait psi

deleteDoublons :: World -> World
deleteDoublons [] = []
deleteDoublons (x : xs) = x : deleteDoublons (eliminer x xs)
  where
    eliminer _ [] = []
    eliminer y (z : zs)
      | y == z = eliminer y zs
      | otherwise = z : eliminer y zs

extrait :: Formula -> World
extrait phi = deleteDoublons (extraitAll phi)

{- findWorlds :: Formula -> [World]
findWorlds phi
  | sat (x : xs) phi == True = x : findWorlds phi
  | otherwise = []
  where
    xs = genAllWorlds (extrait phi) -}

findWorlds :: Formula -> [World]
findWorlds phi = findWorldsInter (genAllWorlds (extrait phi)) phi
  where
    findWorldsInter [] _ = []
    findWorldsInter (x : xs) phi
      | sat x phi == True = x : findWorldsInter xs phi
      | otherwise = findWorldsInter xs phi
