module Cha6(challenge6) where 

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