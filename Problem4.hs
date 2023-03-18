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

interp _ = []

indis "a" 01 = [01]
indis "a" 10 = [10,12]
indis "a" 12 = indis "a" 10
indis "a" 21 = [12,23]
indis "a" 23 = [12,23]
indis "a" 32 = indis "a" 23
indis "a" 34 = [34,23]
indis "a" 43 = indis "a" 34


indis "b" 10 = [10]
indis "b" 01 = [01,12]
indis "b" 21 = [01,21]
indis "b" 12 = [12,32]
indis "b" 23 = [23,43]
indis "b" 32 = [32,12]
indis "b" 43 = [23,43]
indis "b" 34 = [34]

s0 :: EpiState
s0 = (interp , indis ,12)

anneIgn :: EpiFormula 
anneIgn = Not (Knows "a" (Var "b"))

billIgnorance :: EpiFormula
billIgnorance = Not (Knows "b" (Var "a"))

problem4 :: EpiFormula
problem4 = After(
        After
        (And (anneIgn) (billIgnorance)) 
        (Not (anneIgn))
    )(Not (billIgnorance))