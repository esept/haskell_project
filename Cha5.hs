import CPL

-- Choisis bien ta cellule ça’a a de l’importance !
door1 :: Formula
door1 = Or (Not (Var "t1")) (Not (Var "t2")) 

-- tu ferais mieux de choisir l’autre cellule !
door2 :: Formula
door2 = (Var "p1") 

constraint :: Formula
constraint = And (Not (And (Var "p1") (Var "t1"))) (Not (And (Var "p2") (Var "t2")))

constraintVide :: Formula
constraintVide = And (Or (Var "p1") (Var "t1")) (Or (Var "p2") (Var "t2"))

reglement :: Formula
reglement = And (Eqv (door1) (Var "p1")) (Eqv (door2) (Var "t2"))

challenge5 :: Formula
challenge5 = And constraint (And constraintVide reglement)
