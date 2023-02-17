--  On fait l'import du fichier CPL

import CPL (Formula(Var,T,F,And,Or,Imp,Eqv,Not),World,sat,genAllWorlds,findWorlds,constraint,allTests,testAll)


{-  af1 -> affiche 1
Cest une formule logique qui représente: Soit un tigre dans la première cellule
Soit une princesse dans la deuxième
-}
af1 :: Formula 

af1 = (Or (Var "t1") (Var "p2"))


{-  af2 -> affiche 2
Cest une formule logique qui représente une princesse dans la première cellule
-}
af2 :: Formula

af2 = (Var "p1") 


{- constraint2 représente le fait qu'il doit y avoir
 quelque chose derrière chaque porte
-}
constraint2 :: Formula 

constraint2 = And (Or (Var "t2")(Var "p2")) (Or (Var "t1") (Var "p1"))


{- Soit les deux affiches disent la vérité 
soit les deux elles mentent-}
reglement :: Formula

reglement = (Or (And (af1) (af2)) (And (Not (af1)) (Not (af2))) )

{- challenge3 est la conjonction des trois:
reglement, constraint  et  constraint2 
-}
challenge3 :: Formula

challenge3 = (And (And (reglement) (constraint)) (constraint2))

{-Pour trouver tous les mondes qui satisfont la formule challenge3
on va appeler findWorlds de challenge3-}

testChallenge3 = [findWorlds challenge3 == [["p2","p1"]]]