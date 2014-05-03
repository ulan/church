module RawExpr where
import qualified Data.Set as Set

data RawExpr = RawVar String
             | RawApp RawExpr RawExpr
             | RawLam String RawExpr
    deriving (Eq, Ord, Show)

pretty :: RawExpr -> String
pretty (RawVar s) = s
pretty (RawApp x (y@(RawApp _ _))) = pretty x ++ " (" ++ pretty y ++ ")"
pretty (RawApp x y) = pretty x ++ " " ++ pretty y
pretty (RawLam x y) = "(\\" ++ x ++ " -> " ++ pretty y ++ ")"

freeVars :: RawExpr -> Set.Set String
freeVars (v@(RawVar s)) = Set.singleton s
freeVars (RawApp x y) = Set.union (freeVars x) (freeVars y)
freeVars (RawLam x y) = Set.delete x (freeVars y)
