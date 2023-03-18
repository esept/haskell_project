{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

-- La creation du module
module EL (
            epiSat,
            Prop,
            Agent,
            World,
            EpiFormula(T,F,And,Or,Not,Eqv,Imp,Knows,After,Var),
            EpiState,
            test,
            testAll,
            testAppartient
          ) 
          
          where



-- Les trois nouveaux types 
type Prop = [Char]

type Agent = [Char]

type World = Int


{- Le type EpiFormula est utilisé pour exprimer les propositions logiques
avec After et Know nouveaux types pour répresenter les etats epistemiques
-}
data EpiFormula =
                T                               |
                F                               |
                And (EpiFormula) (EpiFormula)   |
                Or (EpiFormula) (EpiFormula)    |
                Not (EpiFormula)                |
                Eqv (EpiFormula) (EpiFormula)   |
                Imp (EpiFormula) (EpiFormula)   |
                Knows String (EpiFormula)       |
                After (EpiFormula) (EpiFormula) |
                Var String
                deriving (Eq,Show,Ord)


{-  Epistate est un tuple de 3 elements:
une fonction qui nous aide à interpreter les variables (Var "as" par exemple)
une fonction qui nous aide à generer les mondes indiscernables par rapport à un agent
un monde (le monde réel)-}

type EpiState = ((Prop -> [World]),(Agent -> World -> [World]),World)

{-  La fonction update va retourner le nouveau EpiState en supprimant en supprimant
tous les mondes possibles qui ne vérifie pas la formule 
-}
update :: EpiState -> EpiFormula -> EpiState

update (int,acc,w) phi  = 
    let updatedInt p    = filter (\x -> epiSat(int,acc,x) phi) (int p)    -- la nouvelle fonction interp 
        updatedAcc i w' = filter(\x -> epiSat (int,acc,x) phi)(acc i w')  -- la nouvelle fonction indis
    in (updatedInt,updatedAcc,w)


{-  La fonction appartient va retourner True (Bool) si le monde appartient
à la liste des mondes et False(Bool) sinon.
-}
appartient :: Int -> [Int] -> Bool
appartient _ [] = False
appartient w (x:xs)
    | x == w = True 
    | otherwise = appartient w xs

testAppartient :: [Bool]
testAppartient = [ appartient 00 [01,11] == False,
                   appartient 11 [11,01,00] == True
                 ]


{-  La fonction epiSat va calculer la valeur de vérité d'une formule par rapport à un 
état epistemique.
On a ajouté des cas specials pour After et Knows

Knows (agent) (formule) va retourner vrai quand phi est vrai dans le monde réel et agent a
n'a plus de doute sur la valeur de phi

After (phi) (psi) va retourner l'etat epistemique satisfait la formule phi et
la mise à jour de l'etat epistemique satisfait aussi la formule psi

Les autres cas sont pareils And,Or,Not,Eqv,Imp
-}
epiSat :: EpiState -> EpiFormula -> Bool 

epiSat _ T                                                                          = True  -- le terminal
epiSat _ F                                                                          = False -- le terminal 
epiSat (inter,acc,w) (Var p)
    | appartient w (inter p)                                                        = True
    | otherwise                                                                     = False
epiSat (state) (And phi psi)
    | epiSat state phi == True && epiSat state psi == True                          = True
    | otherwise                                                                     = False
epiSat (state) (Or phi psi)
    | epiSat state phi == True || epiSat state psi == True                          = True
    | otherwise                                                                     = False
epiSat (state) (Not phi)
    | epiSat state phi == True                                                      = False
    | otherwise                                                                     = True
epiSat (state) (Imp phi psi)
    | epiSat state phi == True && epiSat state psi == True                          = True
    | otherwise                                                                     = False
epiSat (state) (Eqv phi psi)
    | epiSat state phi == epiSat state psi                                          = True
    | otherwise                                                                     = False
epiSat (int,acc,w) (Knows i phi)                                                    = all(\w' -> epiSat(int,acc,w') phi) (acc i w)
epiSat (int,acc,w) (After phi psi)
    | epiSat (int,acc,w) phi == True && epiSat (update (int,acc,w) phi) psi == True = True
    | otherwise                                                                     = False

{-  On va tester la fonction epiSat dans les fichiers ProblemN.hs-}

test :: [Bool] -> Bool

test [] = True 
test (x:xs) = x && test xs

testAll :: [[Bool]] -> [Char]

testAll [] = "Succes!"
testAll (x:xs)
    | test x == False = "Fail!"
    | otherwise = testAll xs