module Cha3(challenge3) where 

import CPL

door1 :: Formula
door1 = Or (Var "t1") (Var "p2") -- porte 1

door2 :: Formula
door2 = (Var "p1") -- porte 2

constraint :: Formula
constraint = And (Not (And (Var "p1") (Var "t1"))) (Not (And (Var "p2") (Var "t2")))

constraintVide :: Formula
constraintVide = Not (And (And (Not (Var "p1")) (Not (Var "t1"))) (And (Not (Var "p1")) (Not (Var "t1"))))

reglement :: Formula
reglement = Eqv (door1) (door2)

challenge3 :: Formula
challenge3 = And constraint (And reglement constraintVide)
