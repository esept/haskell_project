import CPL

type Prop = String
type Agent = String
type World = Int


type EpiState = (Prop->[World],Agent->World->[World],World)

data EpiFormula
  = Var String
  | T
  | F
  | And (EpiFormula) (EpiFormula)
  | Or (EpiFormula) (EpiFormula)
  | Not (EpiFormula)
  | Imp (EpiFormula) (EpiFormula)
  | Eqv (EpiFormula) (EpiFormula)
  | Knows Agent (EpiFormula)
  | After (EpiFormula) (EpiFormula)
  deriving (Show, Eq)

-- e1 = Knows "a" (Not (Knows "b" (Var "as")))
-- e2 =  Knows "a" (Knows "b" (Not (Knows "a" (Var "as"))))

epiSat :: EpiState -> EpiFormula -> Bool
epiSat s phi 
  | sat s phi = True
  | otherwise = False 

sat :: World -> EpiFormula -> Bool
sat _ T = True
sat _ F = False
sat [] (Var f) = False
sat (x : xs) (Var chaine)
  | x == chaine = True
  | otherwise = sat xs (Var chaine)
sat (xs) (Not phi) = not (sat xs phi)
sat s Knows 

update :: EpiState -> EpiFormula -> EpiState
update s 