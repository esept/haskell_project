module Problem1 where 
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
s0 = (interp , indis , 01)

fatherAnn :: EpiFormula
fatherAnn = Or (And (Var "as") (Not(Var "bs")))(And (Not (Var "as"))(Var "bs"))

aliceIgn :: EpiFormula
aliceIgn = And (Not (Knows "a" (Var "as")))(Not (Knows "a" (Not(Var "as"))))

bobIgn :: EpiFormula
bobIgn = And (Not (Knows "b" (Var "bs")))(Not (Knows "b" (Not(Var "bs"))))

problem1 :: EpiFormula
problem1 = After 
    (After 
        (And (fatherAnn) (And (aliceIgn) (bobIgn))) 
        (Not (bobIgn)) 
    ) 
    (Not (aliceIgn))

-- Les deux agents sont ignorants, et (And AliceIng bobIgn)
-- Après l’annonce du père, Alice est ignorante, et 
-- après l’annonce que Bob sait que son propre visage est sale, 
-- Alice et Bob ne sont pas ignorants. »
-- 
-- Note : L’annonce que Bob sait que son propre visage 
-- n’est pas sale est fait implicitement le moment 
-- où il part laver son visage sans même pas regarder le miroir.