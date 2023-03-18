module Problem2 where 
import EL

interp :: Prop -> [World]
interp "as" = [10,11]
interp "bs" = [01,11]
interp _ = []

indis :: Agent -> World -> [World]
indis "a" 00 = [00,10]
indis "a" 10 = [00,10]
indis "a" 01 = [01,11]
indis "a" 11 = [01,11]

indis "b" 00 = [00,01]
indis "b" 01 = [00,01]
indis "b" 10 = [10,11]
indis "b" 11 = [10,11]
indis _ _  = []

s0 :: EpiState
-- s0 = (interp,indis,01)

fatherAnn :: EpiFormula 
fatherAnn = Or (Var "as") (Var "bs")

aliceIgn :: EpiFormula
aliceIgn = And (Not (Knows "a" (Var "as")))(Not (Knows "a" (Not(Var "as"))))

bobIgn :: EpiFormula
bobIgn = And (Not (Knows "b" (Var "bs")))(Not (Knows "b" (Not(Var "bs"))))

problem2 :: EpiFormula
problem2 = After 
    (After 
        (And (fatherAnn) (And (aliceIgn) (bobIgn)))  -- 初始状态
        (And (Not aliceIgn) (Not (bobIgn))) -- 最后状态
    ) 
    (And (aliceIgn) (bobIgn)) -- 中间态1 
