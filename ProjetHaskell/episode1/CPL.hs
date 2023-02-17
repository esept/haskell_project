module CPL
  ( Formula (..),
    World,
  )
where

data Formula
  = Var String
  | T
  | F
  | And (Formula) (Formula)
  | Or (Formula) (Formula)
  | Not (Formula)
  | Imp (Formula) (Formula)
  | Eqv (Formula) (Formula)
  deriving (Show, Eq)

type World = [String]
