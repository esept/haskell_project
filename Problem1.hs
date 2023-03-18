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


testEpiSat = [epiSat s0 (Not (Knows "a" (Var "as"))) == True,
              epiSat s0 (Knows "a" (And (Var "as") (Var "bs"))) == False,
              epiSat s0 (Knows "a" (Not (Knows "b" (Var "as")))) == False
             ]

➜  pro_2 ghci 
GHCi, version 9.2.4: https://www.haskell.org/ghc/  :? for help
ghci> :l Problem1.hs 
[1 of 2] Compiling EL               ( EL.hs, interpreted )

EL.hs:62:24: error:
    • Couldn't match type: [Char]
                     with: Prop -> [World]
      Expected: Prop -> [World]
        Actual: String
    • In the expression: "as"
      In the first argument of ‘epiSat’, namely ‘("as", "a", 01)’
      In the first argument of ‘(==)’, namely
        ‘epiSat ("as", "a", 01) (Not (Knows "a" (Var "as")))’
   |
62 | testEpiSat = [epiSat  ("as" , "a" , 01) (Not (Knows "a" (Var "as"))) == True,
   |                        ^^^^

EL.hs:62:31: error:
    • Couldn't match type: [Char]
                     with: Agent -> World -> [World]
      Expected: Agent -> World -> [World]
        Actual: String
    • In the expression: "a"
      In the first argument of ‘epiSat’, namely ‘("as", "a", 01)’
      In the first argument of ‘(==)’, namely
        ‘epiSat ("as", "a", 01) (Not (Knows "a" (Var "as")))’
   |
62 | testEpiSat = [epiSat  ("as" , "a" , 01) (Not (Knows "a" (Var "as"))) == True,
   |                               ^^^

EL.hs:63:24: error:
    • Couldn't match type: [Char]
                     with: Prop -> [World]
      Expected: Prop -> [World]
        Actual: String
    • In the expression: "as"
      In the first argument of ‘epiSat’, namely ‘("as", "a", 01)’
      In the first argument of ‘(==)’, namely
        ‘epiSat ("as", "a", 01) (Knows "a" (And (Var "as") (Var "bs")))’
   |
63 |               epiSat  ("as" , "a", 01) (Knows "a" (And (Var "as") (Var "bs"))) == False,
   |                        ^^^^

EL.hs:63:31: error:
    • Couldn't match type: [Char]
                     with: Agent -> World -> [World]
      Expected: Agent -> World -> [World]
        Actual: String
    • In the expression: "a"
      In the first argument of ‘epiSat’, namely ‘("as", "a", 01)’
      In the first argument of ‘(==)’, namely
        ‘epiSat ("as", "a", 01) (Knows "a" (And (Var "as") (Var "bs")))’
   |
63 |               epiSat  ("as" , "a", 01) (Knows "a" (And (Var "as") (Var "bs"))) == False,
   |                               ^^^

EL.hs:64:24: error:
    • Couldn't match type: [Char]
                     with: Prop -> [World]
      Expected: Prop -> [World]
        Actual: String
    • In the expression: "as"
      In the first argument of ‘epiSat’, namely ‘("as", "a", 01)’
      In the first argument of ‘(==)’, namely
        ‘epiSat ("as", "a", 01) (Knows "a" (Not (Knows "b" (Var "as"))))’
   |
64 |               epiSat  ("as" , "a" , 01) (Knows "a" (Not (Knows "b" (Var "as")))) == False
   |                        ^^^^

EL.hs:64:31: error:
    • Couldn't match type: [Char]
                     with: Agent -> World -> [World]
      Expected: Agent -> World -> [World]
        Actual: String
    • In the expression: "a"
      In the first argument of ‘epiSat’, namely ‘("as", "a", 01)’
      In the first argument of ‘(==)’, namely
        ‘epiSat ("as", "a", 01) (Knows "a" (Not (Knows "b" (Var "as"))))’
   |
64 |               epiSat  ("as" , "a" , 01) (Knows "a" (Not (Knows "b" (Var "as")))) == False
   |                               ^^^
Failed, no modules loaded.
ghci> 
