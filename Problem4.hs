module Problem4 where 
import EL

interp "a0" = [01]
interp "a1" = [10,12]
interp "a2" = [21,23]
interp "a3" = [32,34]
interp "a4" = [43]

interp "b0" = [10]
interp "b1" = [01,21]
interp "b2" = [12,32]
interp "b3" = [23,43]
interp "b4" = [34]


indis "a" 01 = [01]
indis "a" 10 = [10,12]

indis "a" 12 = indis "a" 10
indis "a" 21 = [12,23]

indis "a" 23 = [12,23]
indis "a" 32 = indis "a" 23

indis "a" 34 = [34,23]
indis "a" 43 = indis "a" 34


indis "b" 10 = [01]
indis "b" 01 = [01,12]
indis "b" 21 = [01,21]





anneIgn :: EpiFormula 
anneIgn = Not (Knows "a" (Var "bs"))

billLinorance :: EpiFormula
billLinorance = Not (Knows "b" (Var "as"))

problem4 :: EpiFormula
problem4 = After (And (anneIgn) (billLinorance)) (Not (anneIgn))