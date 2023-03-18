module EL
  ( Prop,
    Agent,
    World,
    EpiState,
    EpiFormula (..),
    epiSat,
    update,
  )
where

type Prop = String

type Agent = String

type World = Int

type EpiState = (Prop -> [World], Agent -> World -> [World], World)

data EpiFormula
  = Var String
  | T
  | F
  | And (EpiFormula) (EpiFormula)
  | Or (EpiFormula) (EpiFormula)
  | Not (EpiFormula)
  | Imp (EpiFormula) (EpiFormula)
  | Eqv (EpiFormula) (EpiFormula)
  | Knows Agent EpiFormula
  | After (EpiFormula) (EpiFormula)
  deriving (Show, Eq)

contient :: Int -> [Int] -> Bool
contient w [] = False
contient w (x : xs)
  | w == x = True
  | otherwise = contient w xs

test :: [Bool] -> Bool
test [] = True
test (x : xs) = x && test xs

update :: EpiState -> EpiFormula -> EpiState
update (int, acc, w) phi = (updatedInt, updatedAcc, w)
  where
    updatedInt p = filter (\x -> epiSat (int, acc, x) phi) (int p)
    updatedAcc i w' = filter (\x -> epiSat (int, acc, x) phi) (acc i w')

epiSat :: EpiState -> EpiFormula -> Bool
epiSat _ T = True
epiSat _ F = False
epiSat (interp, _, w) (Var p) = contient w (interp p)
epiSat s (And phi psi) = (epiSat s phi) && (epiSat s psi)
epiSat s (Or phi psi) = (epiSat s phi) || (epiSat s psi)
epiSat s (Not phi) = not (epiSat s phi)
epiSat s (Imp phi psi) = not (epiSat s phi) || (epiSat s psi)
epiSat s (Eqv phi psi) = (epiSat s phi) == (epiSat s psi)
epiSat (interp, indis, w) (Knows a phi) = test (map (\wi -> epiSat (interp, indis, wi) phi) (indis a w))
epiSat (int, acc, w) (After phi psi) = (epiSat (int, acc, w) phi) && (epiSat (update (int, acc, w) phi) psi)

testUpdate :: [Bool]
testUpdate = [
    (update (interp,indis,01) (Not (Knows "a" (Var "as")))) == (interp,indis,01) 
  ]
  where
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
    interp :: Prop -> [World]
    interp "as" = [10,11]
    interp "bs" = [01,11]
    interp _ = []


-- testUpdate :: Bool
-- testUpdate = test1 && test2
--   where
--     int1 = [("p", [1,2,3]), ("q", [2,3,4])]
--     acc1 i w = w `elem` i !! iIndex i
--     epiState1 = (int1, acc1, 1)
--     expectedState = (int1, acc1, 1)
--     expectedState' = (int1, acc1', 1)
--       where acc1' i w = w `elem` i !! iIndex i && w `elem` [1,2,3] && not (w `elem` [2,3,4])
--     actualState = update epiState1 (And (Var "p") (Not (Var "q")))
--     test1 = actualState == expectedState'
--     test2 = epiSat actualState (Var "p") == True && epiSat actualState (Var "q") == False


testEpiSat = [ epiSat (interp,indis, 01) (Not (Knows "a" (Var "as"))) == True,
               epiSat (interp,indis, 01) (Knows "a" (And (Var "as") (Var "bs"))) == False,
               epiSat (interp,indis, 01) (Knows "a" (Not (Knows "b" (Var "as")))) == False
             ]
  where
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
    interp :: Prop -> [World]
    interp "as" = [10,11]
    interp "bs" = [01,11]
    interp _ = []




-- testAll = test [
--   test testEpiSat,
--   test testUpdate
--   ]