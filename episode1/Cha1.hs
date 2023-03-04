module Cha1(challenge1) where 
import CPL

-- Il y a une peluche dans cette cellule et un tigre dans l’autre.
door1 :: Formula
door1 = And (Var "p1") (Var "t2") 

-- Il y a une peluche dans une cellule et il y a un tigre dans une cellule.
door2 :: Formula
door2 = And (Or (Var "p1") (Var "p2")) (Or (Var "t1") (Var "t2")) 

-- il ne peut pas y avoir un tigre et une peluche (en même temps) dans chaque cellule
constraint :: Formula
constraint = And (Not (And (Var "p1") (Var "t1"))) (Not (And (Var "p2") (Var "t2")))

-- Une des affiches dit la vérité », promit le roi, « et l’autre ment. »
reglement :: Formula
reglement = Or (And door1 (Not door2)) (And door2 (Not door1))

-- il ne peut pas y avoir une cellule vide 
constraintVide :: Formula
constraintVide = And (Or (Var "p1") (Var "t1")) (Or (Var "p2") (Var "t2"))

challenge1 :: Formula
challenge1 = And (And reglement constraintVide) constraint
