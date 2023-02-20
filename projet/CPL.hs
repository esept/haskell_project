module CPL ( 
  Formula (..), 
  World
) 
where

data Formula = Var String
  | F
  | T
  | Not Formula
  | And Formula Formula
  | Or Formula Formula
  | Imp Formula Formula
  | Eqv Formula Formula
  deriving (Eq, Show, Read)
    
type World = [String]

{-
genEachWorld représente ma fonction intermédiaire, elle permet de concaténer 
chaque chaîne de caractère avec l'ensemble des chaînes restantes de la liste,
ce qui nous permet d'obtenir l'ensembles des mondes possibles.
-}
genEachWorld :: String -> [World] -> [World]
genEachWorld _ [] = []
genEachWorld v (x : xs) = x : (v : x) : genEachWorld v xs

genAllWorlds :: World -> [World]
genAllWorlds [] = [[]]
genAllWorlds [p] = [[], [p]]
genAllWorlds (x : xs) = genEachWorld x (genAllWorlds xs)

-- World = [String] = [[Char]]
-- [World] = [[String]] = [[[Char]]]
-- str = [Char]
-- *Main> createList ["p1", "p2", "p3", "p4"]
-- [[],["p1"],["p1","p2"],["p1","p2","p3"],["p1","p2","p3","p4"],["p1","p2","p4"],["p1","p3"],["p1","p3","p4"],["p1","p4"],["p2"],["p2","p3"],["p2","p3","p4"],["p2","p4"],["p3"],["p3","p4"],["p4"]]
-- testGenAllWorlds


extraitStr :: Formula -> String
extraitStr (Var str) = str
{-
retourne True, si un monde donné contient un paramètre donné,
sinon False.
-}
contains :: [String] -> String -> Bool
contains [] _ = False
contains (w:world) str
    | (w == str) = True
    | otherwise = contains world str

-- sat ["p1", "p2"] (Var "p1") 
sat :: World -> Formula -> Bool
sat [[]] _ = False 
sat _ F = False
sat _ T = True
sat _ (Not F) = True
sat _ (Not T) = False
sat world (Not (Var str)) = ((contains world str) == False)
sat world (Var str) = contains world str

-- And ((Var ...) || T || F)   ((Var ...) || T || F)  
sat world (And a b)
    | (a == F) = False
    | (b == F) = False
    | ((a == T) && (b /= T)) = (contains world (extraitStr b))
    | ((a /= T) && (b == T)) = (contains world (extraitStr a))
    | ((a == T) && (b == T)) = True
sat world (And (Var str1) (Var str2)) = ((contains world str1) && (contains world str2))

-- Or ((Var ...) || T || F)   ((Var ...) || T || F) 
sat world (Or a b)
    | (a == T) = True
    | (b == T) = True
    | ((a == F) && (b /= F)) = (contains world (extraitStr b))
    | ((a /= F) && (b == F)) = (contains world (extraitStr a))
    | ((a == F) && (b == F)) = False
sat world (Or (Var str1) (Var str2)) = ((contains world str1) || (contains world str2))

-- Imp ...; Eqv ...;
sat world (Imp (Var str1) (Var str2)) = contains world str2 -- =? (And (Var str1) (Var str2)) 
sat world (Eqv (Var str1) (Var str2)) = contains world str1 -- sensés être éq (check Eqv if (Var str1) = (Var str2) to be sure)

-- sat ["p1", "p3", "p5", "p7", "p9", "p11"] (Or (Var "p9") (Var "p3")) == True
-- sat ["p2", "p4", "p6", "p8", "p10", "p12"] (Or (Var "p9") (Var "p3")) == False
-- testSat 

{-
retourne un monde (World) extrait à partir de la formula passée en argument.
-}
extrait :: Formula -> World
extrait F = [[]]
extrait T = [[]]
extrait (Not _) = [[]]
extrait (Var str) = [str]
extrait (And (Var str1) (Var str2)) = [str1, str2]
extrait (Or (Var str1) (Var str2))
    | (str1 /= "") = [str1]
    | otherwise = [str2]

-- extrait (And (Var "P1") (Var "P2")) == ["P1","P2"]
-- extrait (Or (Var "P1") (Var "P2")) ["P1"] == ["P1"]
-- extrait (Or (Var "") (Var "P2")) == ["P2"] 
-- testExtrait ... 

-- cherche, un à un, les mondes qui satisfont la formule.
searchWorldOneByOne :: Formula -> [World]-> [World]
searchWorldOneByOne _ [] = []
searchWorldOneByOne phi (w:world) 
    | sat w phi = w : (searchWorldOneByOne phi world)
    | otherwise = searchWorldOneByOne phi world 

findWorlds :: Formula -> [World]
findWorlds phi = searchWorldOneByOne phi (genAllWorlds (extrait phi))

-- genAllWorlds (extrait (And (Var "p1") (Var "P2")))
-- [[],["p1"],["P2"],["p1","P2"]]
-- testFindWorlds