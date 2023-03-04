module Cha3(challenge3) where 

import CPL

-- Il y a un tigre dans cette cellule ou il y a une peluche dans l’autre.
door1 :: Formula
door1 = Or (Var "t1") (Var "p2")

-- Il y a une peluche dans l’autre cellule.
door2 :: Formula
door2 = (Var "p1")

-- il ne peut pas y avoir un tigre et une peluche (en même temps) dans chaque cellule
constraint :: Formula
constraint = And (Not (And (Var "p1") (Var "t1"))) (Not (And (Var "p2") (Var "t2")))

-- il ne peut pas y avoir une cellule vide 
constraintVide :: Formula
constraintVide = And (Or (Var "p1") (Var "t1")) (Or (Var "p2") (Var "t2"))

-- Les affiches disent toutes les deux la vérité ou bien mentent toutes les deux.
reglement :: Formula
reglement = Eqv (door1) (door2)

challenge3 :: Formula
challenge3 = And constraint (And reglement constraintVide)
