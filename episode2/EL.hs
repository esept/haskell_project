module EL
  ( Prop,
    Agent,
    World,
    EpiState,
    EpiFormula (..),
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
