module Main where
import Control.Applicative
import Data.List
import Desugar
import Parser
import RawExpr
import Util
import qualified Data.Map as Map
import qualified Data.Set as Set

data Expr = Var Int
          | App Expr Expr
          | Lam Expr
    deriving (Eq, Ord, Show)

raw :: [String] -> Expr -> RawExpr
raw vars (Var i) = RawVar (vars !! i)
raw vars (App x y) = RawApp (raw vars x) (raw vars y)
raw vars (Lam y) = RawLam x (raw (x:vars) y)
   where x = fresh (Set.fromList vars)
         fresh :: Set.Set String -> String
         fresh forbidden = head [x | x <- generate, not (Set.member x forbidden)]
         generate = [x : xs | xs <- ("":generate), x <- alphabet]
         alphabet = ['a' .. 'z']

debruijn :: (Map.Map String Int) -> Int -> RawExpr -> Expr
debruijn env depth (RawVar s) =
    case Map.lookup s env of
        Just i -> Var $ depth - 1 - i
        Nothing -> error $ "undefined variable " ++ (show s)
debruijn env depth (RawApp x y) = App (debruijn env depth x) (debruijn env depth y)
debruijn env depth (RawLam x y) = Lam (debruijn (Map.insert x depth env) (depth + 1) y)

mapfree f = replace 0
  where replace depth (Var i) | i < depth = Var i
                              | otherwise = f depth i
        replace depth (App x y) = App (replace depth x) (replace depth y)
        replace depth (Lam y) = Lam (replace (depth + 1) y)

add n = mapfree (\depth i -> Var $ i + n)
subst z = mapfree (\depth i -> if i == depth then (add (depth + 1) z) else Var i)

beta :: Expr -> Maybe Expr
beta (Var _) = Nothing
beta (App (Lam y) z) = Just $ add (-1) $ subst z y
beta (App x z) = (flip App z <$> beta x) <|> (App x <$> beta z)
beta (Lam y) = Lam <$> beta y

reductions ((name, expr), desugared) = report name (map pretty (expr : desugared : steps))
  where steps = take 1 $ reverse $ map (raw []) $ reduce beta (debruijn Map.empty 0 desugared)

main = do 
    str <- getContents
    case parseRawExprs str of
        Left err -> print err
        Right xs -> mapM_ (putStrLn . reductions) (zip xs (desugar xs))
