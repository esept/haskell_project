import CPL
------------------------------------------------------------------------------

door1 :: Formula
door1 = And (Var "p1") (Var "t2")

door2 :: Formula 
door2 = And (Or (Var "p1") (Var "p2")) (Or (Var "t1") (Var "t2"))

constraint :: Formula 
constraint = And (Not(And (Var "p1") (Var "t1"))) (Not(And (Var "t2") (Var "p2")))  

reglement :: Formula
reglement = Or (And (Not door1) door2) (And door1 (Not door2))

challenge1 :: Formula
challenge1 = And reglement constraint
------------------------------------------------------------------------------

w0 :: World
w0 = ["p1","p2"]

w1 :: World
w1 = ["t1","t2"]

w2 :: World
w2 = ["p1","p2","t1","t2"]
------------------------------------------------------------------------------

genOne :: [String] -> [World]
genOne [] = []
genOne (w:ws) = [w] : genOne ws

ajoutAll :: World -> [World] -> [World]
ajoutAll _ [] = []
ajoutAll a (b:bs) = ((a++b) :(ajoutAll a bs))

genAll :: [World] -> [World]
genAll (a:[]) = []
genAll (w:ws) = (genAll ws ) ++ (ajoutAll w ws) ++ (ajoutAll w (genAll ws))

genAllWorlds:: [String] -> [World]
genAllWorlds w = (genOne w ) ++ genAll (genOne w) ++ [[]]


test1 = ["a","b","c","d"]
test2 = [["a"],["b"],["c"],["d"]]
test3 = [["e","a"],["e","b"],["e","c"],["e","d"]]
------------------------------------------------------------------------------

countWorld :: [World] -> Int
countWorld [] = 0
countWorld (w:ws) = 1 + countWorld ws
------------------------------------------------------------------------------


sat :: World -> Formula -> Bool
sat _ T = True
sat _ F = False
sat w (Not f) = not(sat w f)
sat [] _ = False
sat (w:ws) (Var f)
    | w == f = True
    | otherwise = sat ws (Var f)
sat ws (And f1 f2) = (sat ws f1) && (sat ws f2)
sat ws (Or f1 f2) = (sat ws f1) || (sat ws f2)
sat ws (Imp f1 f2)
    | (sat ws f1) == True && (sat ws f2) == False = False
    | otherwise = True
sat ws (Eqv f1 f2) = (sat ws f1) == (sat ws f2)

------------------------------------------------------------------------------
findWorlds :: Formula -> [World]
findWorlds w = genAllWorlds(extrait w )

extrait T = []
extrait F = []
extrait (Var w) = w : []
extrait (Not f) = extrait f
extrait (And f1 f2) = (extrait f1) ++ (extrait f2) 
extrait (Imp f1 f2) = (extrait f1) ++ (extrait f2) 
extrait (Or f1 f2) = (extrait f1) ++ (extrait f2) 
extrait (Eqv f1 f2) = (extrait f1) ++ (extrait f2) 

------------------------------------------------------------------------------
challengeN :: Formula
