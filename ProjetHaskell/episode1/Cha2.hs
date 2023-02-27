--import CPL

--genAllWorlds :: World -> [World]
genAllWorlds [] = [[]]
genAllWorlds [p] = [[], [p]]
genAllWorlds (x : xs) = genChaqueWorlds x (genAllWorlds xs)

{-
genChaqueWorlds représente ma fonction intermédiaire, elle permet de concactener chaque chaine
                de caractère avec l'ensemble des chaines restantes de la liste. Ce qui nous permet d'obtenir l'ensembles des mondes possibles

-}
--genChaqueWorlds :: String -> [World] -> [World]
genChaqueWorlds _ [] = []
genChaqueWorlds v (x : xs) = x : (v : x) : genChaqueWorlds v xs