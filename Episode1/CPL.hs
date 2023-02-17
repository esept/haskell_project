{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

{-  Ce projet a été realisé par : Antonio IORGA, Walid JAWAHIR et Ismail AZIZ-}


module CPL (satWorld,satTF,Formula(Var,T,F,And,Or,Imp,Eqv,Not),World,satEvaluate,sat,
inclusion,genAllWorlds,extract,findWorlds,constraint,
extractFiltre,extractAll,contains,containsList,equals,testAll,allTests) where
import Data.List

{- Un nouveau type qui va nous aider à gérer les challenges 
Exemple: And (Or (Var "p1") (Var "p2")) (Or (Var "t1") (Var "t2"))
-}
data Formula 
    = Var String 
    | T 
    | F 
    | And (Formula ) (Formula )
    | Or (Formula ) (Formula)
    | Imp (Formula ) (Formula )
    | Eqv (Formula ) (Formula )
    | Not (Formula )
    deriving (Read,Eq,Show)


-- Un nouveau type    Exemple: ["p1","p2"]
type World  = [String]


{-  Une formule pour verifier qu'il n'y a pas un tigre et une princesse dans la même cellule
-}
constraint :: Formula
constraint = Not (Or (And (Var "p1") (Var "t1")) (And (Var "p2") (Var "t2")))



{-Une fonction pour verifier si une liste elle a toutes les elements du'une autre liste
Exemple :  contains ["p1"] ["p1","p2"] va retourner True 
           contains ["p1","p2"] ["p1"] va retourner False
-}
contains :: World -> World -> Bool

contains [] y = True
contains (x:xs) y = elem x y && contains xs y



{- Cette fonctions va retourner True si deux elements ont les mêmes elements
(même si ils sont pas en ordre) et False sinon
Exemple: equals ["p2","p1"] ["p1","p2"] va retourner True
         equals ["p1"] ["t1"] va retourner False
-}
equals :: [String] -> [String] -> Bool
equals x y = contains x y && contains y x

testEquals :: [Bool]
testEquals = [ equals ["p2","p1"] ["p1","p2"] == True,
               equals ["p2"] ["p1"] == False]

{- Cette fonction va retourner True si la liste des mondes contient un autre monde-}
containsList :: [World] -> World -> Bool 

containsList [] x = False
containsList (y:ys) x
    | (equals x y) == True = True
    | otherwise            = containsList ys x

testContainsList :: [Bool]
testContainsList = [ containsList [["p2","p1"],["p1","p2"]] ["p1","p2"] == True,
                     containsList [["p2","p1"],["p1","p2"]] ["t1"] == False]

{- La fonction 'inclusion' va introduire un element dans toutes les listes d'une liste
Exemple:  inclusion "p1" [["p2"],[]] va retourner [["p2","p1"],["p1"]]
-}
inclusion :: String -> [World] -> [World]

inclusion st [] = []
inclusion st (x:xs) = (conc) : (inclusion st xs) 
                    where conc = st : x


testInclusion :: [Bool]
testInclusion = [ inclusion "p1" [["p2"],[]] == [["p1","p2"],["p1"]] ]

{-  Cette fonction va generer l'ensemble de sous-ensmbles
Exemple de principe pour ["p1","p2","p3"] :
[[]] -> [[p3],[]] -> [["p2"],["p2","p3"],[p3],[]] -> [["p3","p2","p1"],["p2","p1"],["p3","p1"],["p1"],["p3","p2"],["p2"],[p3],[]]
-}
genAllWorlds :: World -> [World]

genAllWorlds [] = [[]]
genAllWorlds (x:xs) = inclusion x (genAllWorlds xs) ++ genAllWorlds xs

w0 :: World
w0 = ["p1","p2"]

w1 :: World
w1 = ["t1","t2"]

w2 :: World 
w2 = ["p1","p2","t1","t2"]

testGenAllWorlds :: [Bool]
testGenAllWorlds = [containsList (genAllWorlds w2) w1 == True,
                    containsList (genAllWorlds w2) w0 == True,
                    containsList (genAllWorlds w2) [] == True,
                    containsList (genAllWorlds w2) ["p1","t5"] == False,
                     containsList (genAllWorlds w2) ["p1","p2","p3"] == False
                    ]

{- | 'satWorld' est une fonction qui va verifier si une variable propositionnelle
appartient à un monde.
Exemple:  satWorld (["p1","p2"],"p1") va retourner T
          satWorld (["p1","p2"],"t1") va retourner F
-}
satWorld :: (World ,String) -> Formula

satWorld ([],var) = F
satWorld ((x:xs),var)
    | x == var  = T
    | otherwise = satWorld (xs,var) 


testSatWorld :: [Bool]
testSatWorld = [satWorld (w2,"p1") == T,
                satWorld (w2,"t3") == F,
                satWorld (w2,"") == F]

{-Le principe de satTF est de remplacer toutes le variables propositionnelles (Var "p1" par exemple) dans une formule avec la valeur T ou F
pour pouvoir utiliser satEvaluate pour evaluer l'expression
Exemple :   satTF ["p1","p2","t1"] And (Var "p1") (Or (Var "t2") (Var "p2")) va retourner And (T) (Or (F) (T))
-}

satTF :: World -> Formula -> Formula

satTF w T = T
satTF w F = F
satTF w (Var p) = satWorld (w,p)
satTF w (And a b)
    | (a == F || a == T) && (b /= T && b /= F)                         = (And a (satTF w b)) 
    | (b == F || b == T) && (a /= T && a /= F)                         = (And (satTF w a) b)
    | otherwise                                                        = (And (satTF w a)(satTF w b))
satTF w (Or a b)
    | (a == F || a == T) && (b /= T && b /= F)                         = (Or a (satTF w b))
    | (b == F || b == T) && (a /= T && a /= F)                         = (Or (satTF w a) b)
    | otherwise                                                        = (Or (satTF w a)(satTF w b))
satTF w (Not a)
    | a == T                                                           = F
    | a == F                                                           = T
    | otherwise                                                        = (Not (satTF w a))


testSatTF :: [Bool]
testSatTF = [satTF ["p1","p2","t1"] (And (Var "p1") (Or (Var "t2") (Var "p2"))) == And (T) (Or (F) (T)),
             satTF ["p1","p2","t1"] (Or (Var "t2") (Var "t3")) == Or (F) (F)]


{- La fonction satEvaluate Va retourner la valeur d'une formule qui n'a pas de variable propositionnelle
Exemple:  satEvaluate And (T) (T) va retourner T
          satEvaluate Or (T) (F) va retourner T
-}
satEvaluate :: Formula -> Formula

satEvaluate T = T
satEvaluate F = F
satEvaluate (And a b)
    | a == T && b == T                                                 = T  -- And T T retourne T
    | (a == F && (b == F || b == T)) || (b == F && (a == T || a == F)) = F  -- And T F ou And F T retourne F
    | (a == F || a == T) && (b /= T && b /= F)                         = satEvaluate (And a f2)  -- And (T ou F) (Formula) va calculer la valeur satEvaluate de Formula avant  
    | (b == F || b == T) && (a /= T && a /= F)                         = satEvaluate (And f1 b)
    | otherwise                                                        = satEvaluate (And f1 f2)  -- And (Formula) (Formula) on calcule les valeurs des deux formules avant
    where f1 = satEvaluate a  -- On a utiliser deux variables pour evaluer récursivement la formule
          f2 = satEvaluate b
satEvaluate (Or a b)
    | a == F && b == F                                                 = F  -- Or F F va retourner F
    | (a == T && (b == F || b == T)) || (b == T && (a == T || a == F)) = T  -- Or F T ou Or T F va retourner T
    | (a == F || a == T) && (b /= T && b /= F)                         = satEvaluate (Or a f2)  -- Or (T ou F) (Formula) va calculer la valeur satEvaluate de Formula avant
    | (b == F || b == T) && (a /= T && a /= F)                         = satEvaluate (Or f1 b)
    | otherwise                                                        = satEvaluate (Or f1 f2)  -- Or (Formula) (Formula) on calcule les valeurs des deux formules avant
    where f1 = satEvaluate a  -- On a utiliser deux variables pour evaluer récursivement la formule
          f2 = satEvaluate b
satEvaluate (Not a)
    | a == T    = F  -- On va retourner l'opposé 
    | a == F    = T
    | otherwise = satEvaluate (Not f1)  -- L'opposé d'une formule
    where f1 = satEvaluate a

testSatEvaluate :: [Bool]
testSatEvaluate = [ satEvaluate (And (T) (T)) == T,
                    satEvaluate (Or (F) (And (T) (F))) == F] 

{- La fonction 'sat' va retourner un bool: True si le monde w satisfait les conditions de la formule phi
                                         False sinon
Exemple: sat ["p1","p2"]  (And (Var "p1") (Var "p2")) va retourner True
on va appeler satTF  ["p1","p2"] (And (Var "p1") (Var "p2")) et ça va retourner (And (T) (T))

après on va appeler satEvaluate (And (T) (T)) et ça va retourner T

Quand la fonction 'satEvaluate' retourne T la fonction 'sat' retourne True
Quand la fonction 'satEvaluate' retourne F la fonction 'sat' retourne False
-}

sat ::  World ->  Formula -> Bool

sat w (phi) 
    | satEvaluate (cond) == T = True 
    | otherwise               = False
    where cond = satTF w (phi)

testSat :: [Bool]
testSat = [ sat ["p1","p2"]  (And (Var "p1") (Var "p2")) == True,
            sat ["p1","p2"] (And (Or (Var "p1") (Var "p2")) (Or (Var "t1") (Var "t2"))) == False,
            sat ["p1","p2"] (And (And (Var "p1") (Var "p2")) (And (Not (Var "p1")) (Not (Var "p2")))) == False]

{- La fonction 'extractAll va retourner une liste avec totues les variables propositionnelles d'une formule 
Exemple: extractAll (Or (And (Var "p1") (Var "p2")) (Var "p1")) va retourner ["p1","p2","p1"]
-}


extractAll :: Formula -> World

extractAll T  = []
extractAll F = []
extractAll (Var b) = [b]
extractAll (And a b) = extractAll a ++ extractAll b
extractAll (Or a b) = extractAll a ++ extractAll b
extractAll (Not a) = extractAll a

testExtractAll :: [Bool]
testExtractAll = [ extractAll (Or (And (Var "p1") (Var "p2")) (Var "p1")) == ["p1","p2","p1"],
                   extractAll (And (Var "p2") (Var "t3")) == ["p2","t3"],
                   extractAll(Or (T) (F)) == [] ]

{- La fonction 'extractFiltre' va enlever tous les doubles d'un monde 
Exemple: extractFiltre ["p1","p2","p1"] va retourner ["p1","p2"]
-}

extractFiltre :: World -> World

extractFiltre [] = []
extractFiltre (x:xs) = x : extractFiltre new 
                     where new = [y | y <- xs, y /= x]


testExtractFiltre :: [Bool]
testExtractFiltre = [ extractFiltre [] == [],
                      extractFiltre ["p1","p2","p1"] == ["p1","p2"]]


{- La fonction 'extract' est juste une combinaison des deux fonctions 'extractFiltre' et 'extractAll'
On a un monde (x:xs) :
à chaque pas on va mettre l'element x dans la nouvelle liste (World :liste de String) et on va enlever tous les x dans la liste xs
et après on repète le processus jusqu'au quand la liste xs est vide
on -}
extract :: Formula -> World

extract phi = extractFiltre (extractAll phi)

testExtract :: [Bool]
testExtract = [ extract (Or (And (Var "p1") (Var "p2")) (Var "p1")) == ["p1","p2"],
               extract (And (Or (T) (F)) (And (T) (F))) == []]


{-  La fonction findWorlds va retourner tous les mondes possibles qui satisfaient la formule phi
On va extraire toutes les variables propositionnelles avec extract
Après on va generer tous les mondes possibles avec cettes variables
Après on va trier la liste ([World]) pour laisser juste les mondes qui satisfaient la formule phi
-}

findWorlds :: Formula -> [World]

findWorlds phi =  lis 
               where lis =  [x | x <- (genAllWorlds (extract phi)), (sat x phi) == True]  -- ici on va trier tous les mondes qui ne satisfait pas la formule

testFindWorlds = [ findWorlds (Or (Var "p1") (Var "p2")) == [["p1","p2"],["p1"],["p2"]],
                   findWorlds (And (Or (Var "p1") (Var "t1")) (Var "p2")) == [["p1","t1","p2"],["p1","p2"],["t1","p2"]]]

test :: [Bool] -> Bool

test [] = True 
test (x:xs) = x && test xs

-- une variable pour repre
testMyFunctions :: [[Bool]]
testMyFunctions = [testEquals,testSatWorld,testContainsList,testExtract,testExtractAll,testExtractFiltre,testInclusion,testSatEvaluate]

testProjectFunctions :: [[Bool]]
testProjectFunctions = [testSat,testGenAllWorlds,testFindWorlds]

allTests :: [[Bool]]
allTests = testMyFunctions ++ testProjectFunctions

-- On va appeler testAll allTests pour voir si toutes les fonctions marchent correctement
testAll :: [[Bool]] -> [Char]

testAll [] = "Succes!"
testAll (x:xs)
    | test x == False = "Fail!"
    | otherwise = testAll xs
                            

