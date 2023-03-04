module Cha4(challenge4) where 
import CPL

-- Choisis n’importe quelle cellule, ça n’a pas d’importance 
door1 :: Formula
door1 = And (Not (Var "t1")) (Not (Var "t2")) 

-- Il y a une peluche dans l’autre cellule.
door2 :: Formula
door2 = (Var "p1") 

-- il ne peut pas y avoir un tigre et une peluche (en même temps) dans chaque cellule
constraint :: Formula
constraint = And (Not (And (Var "p1") (Var "t1"))) (Not (And (Var "p2") (Var "t2")))

-- il ne peut pas y avoir une cellule vide 
constraintVide :: Formula
constraintVide = And (Or (Var "p1") (Var "t1")) (Or (Var "p2") (Var "t2"))

-- l’affiche collé sur la cellule 1 dit la vérité quand il y a une peluche dans cette cellule et ment quand c’est un tigre. Pour la cellule 2 c’est exactement le contraire ; quand il y a une peluche l’affiche ment et quand c’est un tigre l’affiche dit la vérité.
reglement :: Formula
reglement = And (Eqv (door1) (Var "p1")) (Eqv (door2) (Var "t2"))

challenge4 :: Formula
challenge4 = And constraint (And constraintVide reglement)
