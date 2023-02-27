import CPL (Formula (..), World)

contient :: String -> Formula -> Bool
contient xs (Var chaine) = xs == chaine

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

-- findWorlds :: Formula -> [World]
-- extrait :: Formula -> [World]
-- extrait (Var chaine) = genAllWorlds (chaine : [])
