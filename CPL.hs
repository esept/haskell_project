module CPL ( 
  Formula (..),
  World,
  genAllWorlds,
  sat,
  findWorlds
)
where

data Formula = Var String
  | T
  | F
  | And (Formula) (Formula)
  | Or (Formula) (Formula)
  | Not (Formula)
  | Imp (Formula) (Formula)
  | Eqv (Formula) (Formula)
  deriving (Show, Eq)

type World = [String]

{-
genEachWorld représente une fonction intermédiaire, elle permet de concaténer chaque 
chaîne de caractère avec l'ensemble des chaînes restantes de la liste. Ce qui nous permet d'obtenir 
l'ensembles des mondes possibles.
-}
genEachWorld :: String -> [World] -> [World]
genEachWorld _ [] = []
genEachWorld v (x : xs) = x : (v : x) : genEachWorld v xs

testEachWorld = [
    genEachWorld "p1" [] == [],
    genEachWorld "p1" [["p1"]] == [["p1"],["p1","p1"]],
    genEachWorld "p1" [["p1"], ["p2"]] == [["p1"],["p1","p1"],["p2"],["p1","p2"]],
    genEachWorld "p1" [["p1"], ["p2"], ["p3"]] == [["p1"],["p1","p1"],["p2"],["p1","p2"],["p3"],["p1","p3"]]
  ]

genAllWorlds :: World -> [World]
genAllWorlds [] = [[]]
genAllWorlds [p] = [[], [p]]
genAllWorlds (x : xs) = genEachWorld x (genAllWorlds xs)

testGenAllWorlds = [
    -- null (genAllWorlds []),
    -- genAllWorlds [] == [] don't how to make this work,
    genAllWorlds ["p1"] == [[], ["p1"]],
    genAllWorlds ["p1", "p2"] == [[], ["p1"], ["p2"], ["p1", "p2"]],
    genAllWorlds ["p1", "p2", "p3"] == [[],["p1"],["p2"],["p1","p2"],["p3"],["p1","p3"],["p2","p3"],["p1","p2","p3"]],
    genAllWorlds ["p1", "p2", "p3", "p4"] == [[],["p1"],["p2"],["p1","p2"],["p3"],["p1","p3"],["p2","p3"],["p1","p2","p3"],["p4"],["p1","p4"],["p2","p4"],["p1","p2","p4"],["p3","p4"],["p1","p3","p4"],["p2","p3","p4"],["p1","p2","p3","p4"]]
  ] 

deleteDoublons :: World -> World
deleteDoublons [] = []
deleteDoublons (x : xs) = x : deleteDoublons (eliminer x xs)
  where
    eliminer _ [] = []
    eliminer y (z : zs)
      | y == z = eliminer y zs
      | otherwise = z : eliminer y zs

testDeleteDoublons = [
    deleteDoublons [] == [],
    deleteDoublons ["p1", "p1", "p2"] == ["p1", "p2"],
    deleteDoublons ["p1", "p1", "p2", "p3", "p3"] == ["p1", "p2", "p3"]
  ]

extraitAll :: Formula -> World
extraitAll (Var string) = [string]
extraitAll T = []
extraitAll F = []
extraitAll (And phi psi) = extrait phi ++ extrait psi
extraitAll (Or phi psi) = extrait phi ++ extrait psi
extraitAll (Imp phi psi) = extrait phi ++ extrait psi
extraitAll (Eqv phi psi) = extrait phi ++ extrait psi
extraitAll (Not phi) = extrait phi

testExtraitAll = [
    extraitAll T == [], 
    extraitAll F == [], 
    extraitAll (And (Var "t1") (Var "t2")) == ["t1", "t2"],
    extraitAll (Or (Var "t1") (Var "t2")) == ["t1", "t2"],
    extraitAll (Imp (Var "p1") (Var "p2")) == ["p1", "p2"],
    extraitAll (Eqv (Var "p1") (Var "p2")) == ["p1", "p2"],
    extraitAll (Not (Var "p1")) == ["p1"],
    extraitAll (And (Not (Var "p1")) (Not (Var "p2"))) == ["p1", "p2"]
  ]

extrait :: Formula -> World
extrait phi = deleteDoublons (extraitAll phi)

sat :: World -> Formula -> Bool
sat _ T = True
sat _ F = False
sat [] (Var f) = False
sat (x : xs) (Var chaine)
  | x == chaine = True
  | otherwise = sat xs (Var chaine)
sat (xs) (Not phi) = not (sat xs phi)
sat (xs) (And phi psi) = sat (xs) (phi) && sat (xs) (psi)
sat (xs) (Or phi psi) = sat (xs) (phi) || sat (xs) (psi)
sat (xs) (Imp phi psi) = not (sat xs phi) || (sat xs psi)
sat (xs) (Eqv phi psi) = (sat xs phi) == (sat xs psi)

testSat :: [Bool]
testSat = [
    sat []  (Not (Var "p1")) == True,
    sat ["p2"] (Not (Var "p2")) == False,
    sat ["t2"] (Not (Var "p2")) == True,
    sat [] (Imp (Var "t1") (Var "t2")) == True,
    sat ["t1"] (Imp (Var "t1") (Var "t2")) == False,
    sat ["t2"] (Imp (Var "t1") (Var "t2")) == True,
    sat ["t1","t2"] (Imp (Var "t1") (Var "t2")) == True,
    sat [] (Eqv (Var "p1") (Var "t2")) == True,
    sat ["t1","t2"] (Eqv (Var "t1") (Var "t2")) == True,
    sat ["t1"] (Eqv (Var "t1") (Var "t2")) == False,
    sat ["t2"] (Eqv (Var "t1") (Var "t2")) == False,
    sat []  (Or (Var "p1") (Var "p2")) == False,
    sat ["p2"]  (Or (Var "p1") (Var "p2")) == True,
    sat ["p1"]  (Or (Var "p1") (Var "p2")) == True,
    sat ["p1","p2"]  (Or (Var "p1") (Var "p2")) == True,
    sat []  (And (Var "p1") (Var "p2")) == False,
    sat ["p2"]  (And (Var "p1") (Var "p2")) == False,
    sat ["p1"]  (And (Var "p1") (Var "p2")) == False,
    sat ["p1","p2"]  (And (Var "p1") (Var "p2")) == True,
    sat ["p1","p2"] (And (Or (Var "p1") (Var "p2")) (Or (Var "t1") (Var "t2"))) == False,
    sat ["p1","p2"] (And (And (Var "p1") (Var "p2")) (And (Not (Var "p1")) (Not (Var "p2")))) == False
  ]

findWorlds :: Formula -> [World]
findWorlds phi = findWorldsInter (genAllWorlds (extrait phi)) phi
  where
  findWorldsInter [] _ = []
  findWorldsInter (x : xs) phi
    | sat x phi == True = x : findWorldsInter xs phi
    | otherwise = findWorldsInter xs phi

testFindWorlds :: [Bool]
testFindWorlds = [ 
  findWorlds (And (Or (Var "p1")(Var "t1")) (And (Var "p2")(Not (Var "t2")))) == [["p1","p2"],["t1","p2"],["p1","t1","p2"]],
  findWorlds (Or (Var "p1") (Var "p2")) == [["p1"],["p2"],["p1","p2"]],
  findWorlds (And (Or (Var "p1") (Var "t1")) (Var "p2")) == [["p1","p2"],["t1","p2"],["p1","t1","p2"]]
  ]

test [] = True 
test (x:xs) = x && test xs

testAllFunction = [
  test testEachWorld,
  test testGenAllWorlds,
  test testFindWorlds,
  test testExtraitAll,
  test testDeleteDoublons,
  test testSat
  ]

testAll 
  | (test testAllFunction == True) = "Success"
  | otherwise = "Fail"