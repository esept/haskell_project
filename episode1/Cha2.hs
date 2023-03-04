module Cha2(challenge2) where 
import CPL

-- Il y a un tigre dans l’autre cellule.
door1 :: Formula
door1 = Or (Var "p1") (Var "p2")

-- Une au moins de deux cellules contient une peluche.
door2 :: Formula
door2 = (Var "t1")

-- il ne peut pas y avoir une cellule vide 
constraintVide :: Formula
constraintVide = And (Or (Var "p1") (Var "t1")) (Or (Var "p2") (Var "t2"))

--  Elles sont sincères toutes les deux, ou bien elles sont fausses toutes les deux. », affirma le roi.
reglement :: Formula
reglement = Eqv (door1) (door2)

-- Une des affiches dit la vérité », promit le roi, « et l’autre ment. »
constraint :: Formula
constraint = And (Not (And (Var "p1") (Var "t1"))) (Not (And (Var "p2") (Var "t2")))

challenge2 :: Formula
challenge2 = And (And reglement constraintVide) constraint
