module Cha5(challenge5) where 

import CPL

door1 :: Formula
door1 = And (And (Var "t1") (Var "p1")) (And (Var "t2") (Var "p2")) -- porte 1

door2 :: Formula
door2 = And (Var "p1") (Var "t2") -- porte 2

constraint :: Formula
constraint = And (Not (And (Var "p1") (Var "t1"))) (Not (And (Var "p2") (Var "t2")))

constraintVide :: Formula
constraintVide = And (Or (Var "p1") (Var "t1")) (Or (Var "p2") (Var "t2"))

reglement :: Formula
-- reglement = And (And (Var "p1") (Not (Var "t1"))) (And (Var "p2") (Not (Var "t2")))
reglement = And (Eqv (door1) (Var "t1")) (Eqv (door2) (Var "p1"))


-- challenge1 :: Formula
-- challenge1 = And constraint reglement

challenge5 :: Formula
challenge5 = And (And reglement constraint) constraintVide