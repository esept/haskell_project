module Cha5(challenge5) where 

import CPL
-- Choisis bien ta cellule ça’a a de l’importance !
door1 :: Formula
door1 = Or (And (Var "t1")(Not (Var "t2"))) (And (Var "t2")(Not (Var "t1")))

-- Tu ferais mieux de choisir l’autre cellule !
door2 :: Formula
door2 = (Var "p1")  

-- il ne peut pas y avoir un tigre et une peluche (en même temps) dans chaque cellule
constraint :: Formula
constraint = And (Not (And (Var "p1") (Var "t1"))) (Not (And (Var "p2") (Var "t2")))

-- il ne peut pas y avoir une cellule vide 
constraintVide :: Formula
constraintVide = And (Or (Var "p1") (Var "t1")) (Or (Var "p2") (Var "t2"))

--  l’affiche collé sur la cellule 1 dit la vérité quand il y a une peluche dans cette cellule et ment quand c’est un tigre. Pour la cellule 2 c’est exactement le contraire ; quand il y a une peluche l’affiche ment et quand c’est un tigre l’affiche dit la vérité.
reglement :: Formula
reglement = And (Eqv (door1) (Var "p1")) (Eqv (door2) (Var "t2")) 

challenge5 :: Formula
challenge5 = And (And reglement constraint) constraintVide
