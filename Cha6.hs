import CPL

door1 :: Formula
door1 = (Var "t1") 

door2 :: Formula
door2 = Or (Var "p2") (Var "t2")

constraint :: Formula
constraint = And (Not (And (Var "p1") (Var "t1"))) (Not (And (Var "p2") (Var "t2")))

reglement :: Formula
reglement = Or (Eqv (door1) (Var "p1")) (Eqv (door2) (Or (Var "p2") (Var "t2")))

challenge6 :: Formula
challenge6 = And constraint reglement

-- *Main> findWorlds challenge6
-- [[],["p1"],["t1"],["p2"],["p1","p2"],["t1","p2"],["t2"],["p1","t2"],["t1","t2"]]