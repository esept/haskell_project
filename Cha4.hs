module Cha4(challenge4) where 

import CPL

door1 :: Formula
door1 = And (Not (Var "t1")) (Not (Var "t2")) -- porte 1

door2 :: Formula
door2 = (Var "p1") -- porte 2

constraint :: Formula
constraint = And (Not (And (Var "p1") (Var "t1"))) (Not (And (Var "p2") (Var "t2")))

constraintVide :: Formula
constraintVide = And (Or (Var "p1") (Var "t1")) (Or (Var "p2") (Var "t2"))

reglement :: Formula
-- reglement = And (And (Var "p1") (Not (Var "t1"))) (And (Var "t2") (Not (Var "p2")))
reglement = And (Eqv (door1) (Var "p1")) (Eqv (door2) (Var "t2"))

challenge4 :: Formula
challenge4 = And constraint (And constraintVide reglement)
