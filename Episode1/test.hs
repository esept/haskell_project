import System.IO

inclusion :: String -> [[String]] -> [[String]]

inclusion st [] = []
inclusion st (x:xs) = (conc) : (inclusion st xs) where conc = st : x

x :: [[String]]
x = inclusion "p1" [["t","p"],["t"]]
