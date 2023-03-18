module Problem2 where

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

interp "as" = [10,11]
interp "bs" = [01,11]
interp _ = []

testInterp :: [Bool]
testInterp = [interp "as" == [10,11],
              interp "anything" == []]


{-  La fonction indis va envoyer une liste avec tous les mondes indiscenrbales
d'un agent par rapport à un autre monde
-}
indis :: Agent -> World -> [World]

indis "a" 00 = [00,10]
indis "b" 00 = [00,01]
indis "a" 10 = [10,00]
indis "b" 10 = [10,11]
indis "a" 01 = [01,11]
indis "b" 01 = [01,00]
indis "a" 11 = [11,01]
indis "b" 11 = [11,10]
indis  _  _  = []


testIndis :: [Bool]
testIndis = [ indis "a" 10 == [10,00],
              indis "ac" 1 == []]


--  L'état initial de notre problème: tous les deux sont sales
s0 :: EpiState

s0 = (interp,indis,11)


{-  L'annonce du pere :
Au moins un enfant a le visage sale-}
fatherAnn :: EpiFormula 

fatherAnn = (Or (Var "as") (Var "bs"))



{-  Une representation logique de l'énoncé: 
Alice ne sait pas si elle a le visage sale ou pas-}
aliceIgn :: EpiFormula

aliceIgn = ( And (Not (Knows "a" (Var "as")))   (Not (Knows "a" (Not(Var "as")))))


{-  Une representation logique de l'énoncé: 
Bob ne sait pas si elle a le visage sale ou pas-}
bobIgn :: EpiFormula

bobIgn = ( And (Not (Knows "b" (Var "bs")))   (Not (Knows "b" (Not(Var "bs")))))

--  La condition initiale: Une conjonction de fatherAnn, aliceIgn et bobIgn
condInit :: EpiFormula 

condInit = (And (fatherAnn) (And (bobIgn) (aliceIgn)))

{-  Ici on a utiliser la formule after pour faire à chaque fois la mise à jour 
après l'annonce du père. Les enfants au debut: ils savent pas mais après ils savent 
-}
problem2 :: EpiFormula

problem2 = After (After (condInit) (And (bobIgn) (aliceIgn))) (And (Knows "a" (Var "as")) (Knows "b" (Var "bs")))

{- Pour voir si le raisonnement est bon 
on va appeler epiSat s0 probleme2-}

testEpiSat :: [Bool]
testEpiSat = [epiSat s0 (Not (Knows "a" (Var "as"))) == True,
              epiSat s0 (Knows "a" (And (Var "as") (Var "bs"))) == False,
              epiSat s0 (Knows "a" (Not (Knows "b" (Var "as")))) == False
             ]

allTests :: [[Bool]]
allTests = [testEpiSat,testInterp,testIndis,testAppartient]

{-  On doit appeler testAll allTests pour voir
si tous les test on été effectué avec du succes-}