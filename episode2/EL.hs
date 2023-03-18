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
