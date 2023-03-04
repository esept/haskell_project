module Cha6(challenge6) where 
import CPL 

door1::Formula
door1 = (Var "t1") 

door2::Formula
door2 = (Var "p2")

door3 :: Formula
door3 = (Var "t2")

constraint_1:: Formula
constraint_1 = (Not (And (Var "t1") (Var "p1")))
constraint_2:: Formula
constraint_2 = (Not (And (Var "t2") (Var "p2")))
constraint_3:: Formula
constraint_3 = (Not (And (Var "t3") (Var "p3")))
constraint:: Formula
constraint = And (And constraint_1 constraint_2) constraint_3

constraint1:: Formula
constraint1 = And (Var "p1") (And (Var "t2") (Var "t3"))
constraint2:: Formula
constraint2 = And (Var "p2") (And (Var "t1") (Var "t3"))
constraint3:: Formula
constraint3 = And (Var "p3") (And (Var "t2") (Var "t1"))
constraint6:: Formula
constraint6 = Or (constraint1) (Or constraint2 constraint3)

reglement :: Formula
reg6_1 :: Formula
reg6_1 = And (door1) (And (Not (door2)) (Not (door3)))
reg6_2 :: Formula
reg6_2 = And (door2) (And (Not (door1)) (Not (door3)))
reg6_3 :: Formula
reg6_3 = And (door3) (And (Not (door2)) (Not (door1)))
reglement = Or (reg6_1) (Or (reg6_2) (reg6_3))


challenge6 :: Formula
challenge6 = And constraint6 (And constraint reglement)
