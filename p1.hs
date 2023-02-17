-- data Formula = Var String
--     | T 
--     | F 
--     | Not Formula
--     | And Formula Formula
--     | Or Formula Formula
--     | Imp Formula Formula
--     | Eqv Formula Formula
--     deriving (Show,Eq,Read)

-- type World = [String]

import CPL

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


w0 :: World
w0 = ["p1","p2"]

w1 :: World
w1 = ["t1","t2"]

w2 :: World
w2 = ["p1","p2","t1","t2"]

genOne [] = []
genOne (w:ws) = [w] : genOne ws

ajoutAll _ [] = []
ajoutAll a (b:bs) = (a++b) :(ajoutAll a bs)

genAll (a:[]) = []
genAll (w:ws) = (genAll ws ) ++ (ajoutAll w ws) ++ (ajoutAll w (genAll ws))
genAllWorlds w = (genOne w ) ++ genAll (genOne w) ++ [[]]


test1 = ["a","b","c","d"]
test2 = [["a"],["b"],["c"],["d"]]
test3 = [["e","a"],["e","b"],["e","c"],["e","d"]]

countWorld [] = 0
countWorld (w:ws) = 1 + countWorld ws