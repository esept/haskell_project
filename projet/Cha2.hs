import CPL

door1 :: Formula
door1 = Or 
    ( And (Var "p1") (Or (Var "p2") (Var "t2")) )
    ( And (Or (Var "p1") (Var "t1")) (Var "p2") )

door2 :: Formula
door2 = And (Or (Var "p1") (Var "t1")) (Var "t2")

-- ... constraint reglement challenge2