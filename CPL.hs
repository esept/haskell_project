module CPL
(
    Formula(..),
    World
)
where
    data Formula = Var String
        | T 
        | F 
        | Not Formula
        | And Formula Formula
        | Or Formula Formula
        | Imp Formula Formula
        | Eqv Formula Formula
        deriving (Show,Eq,Read)

    type World = [String]