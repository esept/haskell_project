import EL

interp :: Prop -> [World]
interp "as" = [10, 11] -- Alice est sale dans les mondes 10 et 11.
interp "bs" = [01, 11] -- Bob est sale dans les mondes 01 et 11.
interp _ = [] -- Toutes les autres propositions sont fausses.

indis :: Agent -> World -> [World]
indis "a" 00 = [00, 10] -- Alice ne peut pas distinguer 00 de 10.
indis "b" 00 = [00, 01] -- Bob ne peut pas distinguer 00 de 01.
indis "a" 10 = [10, 00] -- Alice ne peut pas distinguer 00 de 10.
indis "b" 10 = [10, 11] -- Bob ne peut pas distinguer 10 de 11.
indis "a" 01 = [01, 11] -- Alice ne peut pas distinguer 01 de 11.
indis "b" 01 = [01, 00] -- Bob ne peut pas distinguer 01 de 00.
indis "a" 11 = [11, 01] -- Alice ne peut pas distinguer 01 de 11.
indis "b" 11 = [11, 10] -- Bob ne peut pas distinguer 10 de 11.
indis _ _ = []

s0 :: EpiState
s0 = (interp, indis, 01)

fatherAnn :: EpiFormula
fatherAnn = Or (And (Var "as") (Not (Var "bs"))) (And (Var "bs") (Not (Var "as")))

aliceIgn :: EpiFormula
aliceIgn = And (Not (Knows "a" (Var "as"))) (Not (Knows "a" (Not (Var "as"))))

bobIgn :: EpiFormula
bobIgn = And (Not (Knows "b" (Var "bs"))) (Not (Knows "b" (Not (Var "bs"))))

problem1 :: EpiFormula
problem1 = After (After (And (fatherAnn) (And (aliceIgn) (bobIgn))) (Not (bobIgn))) (Not (aliceIgn))
