{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Problem3 where

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

interp "as" = [100,110,101,111]
interp "bs" = [010,110,011,111]
interp "cs" = [001,101,011,111]
interp _ = []


testInterp :: [Bool]
testInterp = [interp "as" == [100,110,101,111],
              interp "anything" == []]


{-  La fonction indis va envoyer une liste avec tous les mondes indiscenrbales
d'un agent par rapport à un autre monde
-}
indis :: Agent -> World -> [World]

indis "a" 000 = [000,100]
indis "b" 000 = [000,010]
indis "c" 000 = [000,001]

indis "a" 001 = [001,101]
indis "b" 001 = [001,011]
indis "c" 001 = [001,000]

indis "a" 010 = [010,110]
indis "b" 010 = [010,000]
indis "c" 010 = [010,011]

indis "a" 100 = [100,000]
indis "b" 100 = [100,110]
indis "c" 100 = [100,101]

indis "a" 110 = [110,010]
indis "b" 110 = [110,100]
indis "c" 110 = [110,111]

indis "a" 101 = [101,001]
indis "b" 101 = [101,111]
indis "c" 101 = [101,100]

indis "a" 011 = [011,111]
indis "b" 011 = [011,001]
indis "c" 011 = [011,010]

indis "a" 111 = [111,011]
indis "b" 111 = [111,101]
indis "c" 111 = [111,110]


testIndis = [ indis "a" 111 == [111,011],
              indis "any" 1234 == []]


--  L'état initial de notre problème: tous les trois sont sales
s0 :: EpiState 

s0 = (interp,indis,111)


{-  L'annonce du pere :
Au moins un enfant a le visage sale-}
fatherAnn :: EpiFormula 

fatherAnn = (Or (Var "as") (Or (Var "bs") (Var "cs")))


{-  Une representation logique de l'énoncé: 
Alice ne sait pas si elle a le visage sale ou pas-}
aliceIgn :: EpiFormula

aliceIgn = ( And (Not (Knows "a" (Var "as")))   (Not (Knows "a" (Not(Var "as")))))


{-  Une representation logique de l'énoncé: 
Bob ne sait pas si elle a le visage sale ou pas-}
bobIgn :: EpiFormula

bobIgn = ( And (Not (Knows "b" (Var "bs")))   (Not (Knows "b" (Not(Var "bs")))))


{-  Une representation logique de l'énoncé: 
Caroline ne sait pas si elle a le visage sale ou pas-}
carolineIgn :: EpiFormula

carolineIgn = ( And (Not (Knows "c" (Var "cs")))   (Not (Knows "c" (Not(Var "cs")))))


--  La condition initiale: Une conjonction de fatherAnn, aliceIgn, bobIgn et carolineIgn
condInit :: EpiFormula 

condInit = And (And (fatherAnn) (aliceIgn)) (And (bobIgn) (carolineIgn))


--  Une variable qui exprime le fait qu'aucun d'eux trois s'il a le visage sale ou pas
les3SaventPas :: EpiFormula 

les3SaventPas = And (And (aliceIgn) (bobIgn)) (carolineIgn)


{-  Ici on a utiliser la formule after pour faire à chaque fois la mise à jour 
après l'annonce du père. Les enfants au debut: ils savent pas mais après ils savent 
-}
problem3 :: EpiFormula

problem3 = After (After (After (condInit) (les3SaventPas)) (les3SaventPas))  (And (And (Not (aliceIgn)) (Not (bobIgn))) (Not (carolineIgn)))

{- Pour voir si le raisonnement est bon 
on va appeler epiSat s0 probleme3-}

testEpiSat :: [Bool]
testEpiSat = [epiSat s0 (Not (Knows "a" (Var "as"))) == True,
              epiSat s0 (Knows "a" (And (Var "as") (Var "bs"))) == False,
              epiSat s0 (Knows "a" (Not (Knows "b" (Var "as")))) == False,
              epiSat s0 (Knows "c" (Var "cs")) == False
             ]

allTests :: [[Bool]]
allTests = [testEpiSat,testInterp,testIndis,testAppartient]

{-  On doit appeler testAll allTests pour voir
si tous les test on été effectué avec du succes-}