module Cha5(challenge5) where 

import CPL

door1 :: Formula
door1 = Or (Not (Var "t1")) (Not (Var "t2")) 

door2 :: Formula
door2 = And (Var "p1") (Var "t2")

constraint :: Formula
constraint = And (Not (And (Var "p1") (Var "t1"))) (Not (And (Var "p2") (Var "t2")))

constraintVide :: Formula
constraintVide = And (Or (Var "p1") (Var "t1")) (Or (Var "p2") (Var "t2"))

reglement :: Formula
reglement = And (Eqv (door1) (Var "p1")) (Eqv (door2) (Var "t2"))

challenge5 :: Formula
challenge5 = And (And reglement constraint) constraintVide