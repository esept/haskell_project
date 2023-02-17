--  On fait l'import du fichier CPL

import CPL (Formula(Var,T,F,And,Or,Imp,Eqv,Not),World,sat,genAllWorlds,findWorlds,constraint,allTests,testAll)


{-  af1 -> affiche 1
Cest une formule logique qui représente: 
Il ne peut pas y avoir deux tigres ou deux princesses en même temps
-}
af1 :: Formula

af1 = (Or (And (Var "p1") (Var "t2")) (And (Var "t1") (Var "p2")))


{-  af2 -> affiche 2
Cest une formule logique qui représente une princesse dans la première cellule
-}
af2 :: Formula 

af2 = (Var "p1")


{-  Si'il y a une princesse dans la première cellule af1 dit la vérité
    Sinon af1 ment
-}
reglement1 :: Formula

reglement1 = (Or (And (Var "p1")(af1)) (And (Var "t1") (Not(af1))))


{-  Si'il y a un tigre dans la deuxième cellule af2 dit la vérité
    Sinon af2 ment
-}
reglement2 :: Formula

reglement2 = (Or (And (Var "t2") (af2)) (And (Var "p2")  (Not(af2))))


{- La conjonction des deux reglements-}
reglement :: Formula 

reglement = (And (reglement1) (reglement2))


{- challenge5 est la conjonction des deux:
reglement et constraint 
-}
challenge5 :: Formula 

challenge5 = (And (reglement) (constraint))

{-Pour trouver tous les mondes qui satisfont la formule challenge5
on va appeler findWorlds de challenge5-}

testChallenge5 = [findWorlds challenge5 == [["p1","t2"]]]