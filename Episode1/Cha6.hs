--  On fait l'import du fichier CPL
import CPL (Formula(Var,T,F,And,Or,Imp,Eqv,Not),World,sat,genAllWorlds,findWorlds,allTests,testAll)

{- Une nouvelle contrainte qui verifie qu'il n'y a pas
un tigre et une princesse dans aucune des trois cellules en même temps
-}
constraint :: Formula
constraint = (Not (Or (Or (And (Var "p1") (Var "t1")) (And (Var "p2") (Var "t2"))) (And (Var "p3") (Var "t3"))))


{-  af1 -> affiche 1
Cest une formule logique qui représente: 
Il y a un tigre dans la première cellule
-}
af1 :: Formula

af1 = (Var "t1")


{-  af2 -> affiche 2
Cest une formule logique qui représente une princesse dans la deuxième cellule
-}
af2 :: Formula

af2 = (Var "p2")


{-  af3 -> affiche 3
Cest une formule logique qui représente un tigre dans la deuxième cellule
-}
af3 :: Formula 

af3 = (Var "t2")


{- Une formule logique qui représente que le premier affiche 
dit la vérité et les deux autres mentent
-}
tAf1 :: Formula 

tAf1 = (And (af1) (And (Not (af2)) (Not af3)))


{- Une formule logique qui représente que le deuxième affiche 
dit la vérité et les deux autres mentent
-}
tAf2 :: Formula

tAf2 = (And (Not (af1)) (And (af2) (Not af3)))


{- Une formule logique qui représente que le troisième affiche 
dit la vérité et les deux autres mentent
-}
tAf3 :: Formula 

tAf3 = (And (Not (af1)) (And (Not (af2))  (af3)))


{- Une formule logique qui reprèsente qu'un seul des trois doit être vrai 
entre: tAf1, tAf2 et tAf3-}
reglement :: Formula 

reglement = Or (tAf1) (Or (tAf2) (tAf3)) 

{- constraint2 représente le fait qu'il doit y avoir
 quelque chose derrière chaque porte
-}
constraint2 :: Formula 

constraint2 = Or (And (Var "p1") (And (Var "t2") (Var "t3"))) (Or (And (Var "p2") (And (Var "t1") (Var "t3"))) (And (Var "p3") (And (Var "t2") (Var "t1"))))

{- challenge5 est la conjonction des trois:
reglement, constraint2 et constraint 
-}
challenge6 :: Formula 

challenge6 = (And (And (reglement) (constraint)) (constraint2))

{-Pour trouver tous les mondes qui satisfont la formule challenge6
on va appeler findWorlds de challenge6-}

testChallenge6 = [findWorlds challenge6 == [["t2","p1","t3"]]]

