import CPL

door1 :: Formula
door1 = And (Var "p1") (Var "t2")

door2 :: Formula
door2 = And (Or (Var "p1") (Var "t1")) (Or (Var "p2") (Var "t2")) -- instead of And (Or (Var "p1") (Var "p2")) (Or (Var "t1") (Var "t2"))

constraint :: Formula 
constraint = And (Not (And (Var "p1") (Var "t1"))) (Not (And (Var "p2") (Var "t2"))) 
-- constraint = Or (And (Var "p1") (Var "t2")) (And (Var "t1") (Var "p2")) 

reglement :: Formula 
reglement = Or (And door1 (Not door2)) (And (Not door1) door2)

challenge1 :: Formula 
challenge1 = And constraint reglement