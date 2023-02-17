{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

--  On fait l'import du fichier CPL
import CPL (Formula(Var,T,F,And,Or,Imp,Eqv,Not),World,sat,genAllWorlds,findWorlds,constraint,allTests,testAll)


{-  af1 -> affiche 1
Cest une formule logique qui représente une princesse dans la première cellule et un tigre dans la deuxième
-}
af1 :: Formula

af1  =   (And (Var "p1") (Var "t2")) 

{-  af2 -> affiche 2
Cest une formule logique qui représente: il y a une princesse et un tigre mais on sait pas ou
-}
af2 :: Formula 

af2  = Or ((And (Var "p1")) (Var "t2"))  (And (Var "t1") (Var "p2"))

{- Cette formule veut dire qu'un des deux affiche ment et l'autre il dit la vérité
-}
reglement :: Formula 

reglement  = Or (And (af1) (Not (af2))) (And (Not (af1 )) (af2))

{- challenge1 est la conjonction des deux:
reglement et constraint
-}
challenge1 :: Formula

challenge1 = (And (reglement) (constraint))

{-Pour trouver tous les mondes qui satisfont la formule challenge1
on va appeler findWorlds de challenge1-}

testChallenge1 = [findWorlds challenge1 == [["t1","p2"]]]