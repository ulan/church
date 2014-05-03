module Main where
import Desugar
import Parser
import RawExpr
import Text.ParserCombinators.Parsec
import Util
import qualified Data.Map as Map
import qualified Data.Set as Set

fresh :: Set.Set String -> String
fresh forbidden = head [x | x <- generate, not (Set.member x forbidden)]
  where generate = [x : xs | xs <- ("":generate), x <- alphabet]
        alphabet = ['a' .. 'z']


subst :: String -> RawExpr -> RawExpr -> RawExpr
subst var new (RawVar s) | var == s = new
                      | otherwise = (RawVar s)
subst var new (RawApp x y) = RawApp (subst var new x) (subst var new y)
subst var new (RawLam x y) | var == x = RawLam x y
                           | Set.member x freenew = RawLam x' (subst var new y') 
                           | otherwise = RawLam x (subst var new y) 
  where freenew = freeVars new
        x' = fresh (Set.union (Set.singleton var) (Set.union freenew (freeVars y)))
        y' = subst x (RawVar x') y

beta :: RawExpr -> Maybe RawExpr
beta (RawVar _) = Nothing
beta (RawApp (RawLam x y) z) = Just $ subst x z y
beta (RawApp x z) = maybe (fmap (RawApp x) (beta z)) (Just . flip RawApp z) (beta x)
beta (RawLam x y) = fmap (RawLam x) (beta y) 

reductions ((name, expr), desugared) = report name steps
  where steps = map pretty $ (expr : desugared : reduce beta desugared)

main = do 
    str <- getContents
    case parseRawExprs str of
        Left err -> print err
        Right xs -> mapM_ (putStrLn . reductions) (zip xs (desugar xs))
