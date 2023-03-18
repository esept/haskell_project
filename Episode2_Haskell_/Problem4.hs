{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Problem4 where

import EL (
            epiSat,
            EpiState,
            Prop,
            Agent,
            World,
            EpiFormula(T,F,And,Or,Not,Eqv,Imp,Knows,After,Var),
            test,
            testAll,
            testAppartient
          )



{-  La fonction interp va interpreter une variable Var p
et va envoyer la liste des mondes possibles
-}
interp :: Prop -> [World]

interp "a0" = [01]    -- anne a le numero 0
interp "a1" = [10,12] -- anne a le numero 1 
interp "a2" = [21,23]
interp "a3" = [32,34]
interp "a4" = [43]

interp "b0" = [10]    -- bill a le numero 0
interp "b1" = [01,21]
interp "b2" = [12,32]
interp "b3" = [23,43]
interp "b4" = [34]

interp _ = []


testInterp :: [Bool]
testInterp = [ interp "b1" == [01,21],
               interp "anything" == []
             ]


{-  La fonction indis va envoyer une liste avec tous les mondes indiscenrbales
d'un agent par rapport à un autre monde
-}
indis :: Agent -> World -> [World]

indis "a" 01 = [01]
indis "b" 01 = [01,21]

indis "a" 10 = [10,12]
indis "b" 10 = [10]

indis "a" 12 = [12,10]
indis "b" 12 = [12,32]

indis "a" 21 = [21,23]
indis "b" 21 = [21,01]

indis "a" 23 = [23,21]
indis "b" 23 = [23,43]

indis "a" 32 = [32,34]
indis "b" 32 = [32,12]

indis "a" 34 = [34,32]
indis "b" 34 = [34]

indis "a" 43 = [43]
indis "b" 43 = [43,23]

indis _ _ =[]

testIndis :: [Bool]
testIndis = [ indis "a" 43 == [43],
              indis "a" 32 == [32,34]
            ]


--  L'état initial de notre problème: on peut avoir deux solutions: 21 et 23
s0 :: EpiState

s0 = (interp,indis,21)


{-  Une representation logique de l'énoncé: 
Anne ne sait le nombre de bill-}
anneIgn :: EpiFormula 

anneIgn = And (And (And (Not (Knows "a" (Var "b0"))) (Not (Knows "a" (Var "b1")))) (Not (Knows "a" (Var "b2")))) (And (Not (Knows "a" (Var "b3"))) (Not (Knows "a" (Var "b4"))))


{-  Une representation logique de l'énoncé: 
Bill ne sait le nombre de Anne (si elle a 0 ou 1 ou 2 ou 3 ou 4)-}
billIgn :: EpiFormula 

billIgn = And (And (And (Not (Knows "b" (Var "a0"))) (Not (Knows "b" (Var "a1")))) (Not (Knows "b" (Var "a2")))) (And (Not (Knows "b" (Var "a3"))) (Not (Knows "b" (Var "a4"))))



{-  Ici on a utiliser la formule after pour faire à chaque fois la mise à jour 
après l'annonce du père. Les enfants au debut: ils savent pas le nombre de chacun mais après
chacun annonce son ignorance les deux savent .
-}

problem4 :: EpiFormula

problem4 = After (After (And (anneIgn) (billIgn)) (anneIgn)) (Not (billIgn))

{- Pour voir si le raisonnement est bon 
on va appeler epiSat s0 probleme4-}

testEpiSat :: [Bool]
testEpiSat = [  epiSat s0 billIgn == True,
                epiSat s0 anneIgn == True,
                epiSat s0 (Knows "a" (Var "a2")) == True,
                epiSat s0 (Knows "b" (Var "b1")) == True
             ]

allTests :: [[Bool]]
allTests = [testEpiSat,testInterp,testIndis,testAppartient]

{-  On doit appeler testAll allTests pour voir
si tous les test on été effectué avec du succes-}