module Main where
import qualified Data.Set as Set
import qualified Data.Map as Map
import Text.ParserCombinators.Parsec
import Data.List

data Expr = Var Int
          | App Expr Expr
          | Lam Expr
    deriving (Eq, Ord, Show)

data RawExpr = RawVar String
             | RawApp RawExpr RawExpr
             | RawLam String RawExpr
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

pretty :: RawExpr -> String
pretty (RawVar s) = s
pretty (RawApp x (y@(RawApp _ _))) = pretty x ++ " (" ++ pretty y ++ ")"
pretty (RawApp x y) = pretty x ++ " " ++ pretty y
pretty (RawLam x y) = "(\\" ++ x ++ " -> " ++ pretty y ++ ")"

parseExprs :: Parser [(String, RawExpr)]
parseExprs = spaces >> separatedBy spaces binding
  where binding = do x <- identifier
                     inlineSpaces 
                     char '='
                     inlineSpaces
                     y <- expr inlineSpaces
                     return (x, y)
        expr ws = do ws
                     xs <- separatedBy ws (paren <|> lambda ws <|> var)
                     return (apply xs)
        paren = do char '('
                   inner <- expr spaces -- Allow any spaces within parens.
                   char ')'
                   return inner
        lambda ws = do char '\\'
                       ws
                       vars <- separatedBy ws identifier
                       char '-'
                       char '>'
                       body <- expr ws
                       return (abstract vars body)
        var = identifier >>= return . RawVar
        apply (x : xs) = foldl RawApp x xs
        abstract vars body = foldr RawLam body vars 
        identifier = many1 (letter <|> digit <|> oneOf "_$'")
        inlineSpaces = many (oneOf " \t") >> return ()
        separatedBy sep item = many1 (item >>= \x -> sep >> return x)

freeVars :: RawExpr -> Set.Set String
freeVars (v@(RawVar s)) = Set.singleton s
freeVars (RawApp x y) = Set.union (freeVars x) (freeVars y)
freeVars (RawLam x y) = Set.delete x (freeVars y)

desugar :: [(String, RawExpr)] -> [RawExpr]
desugar = reverse . snd . foldl desugarOne (Map.empty, [])
  where desugarOne (bindings, result) (x, y) = (bindings', result')
          where bindings' = Map.insert x y' bindings
                result' = y' : result
                y' = foldl abstract y (Set.toList $ freeVars y)
                abstract expr s = RawApp (RawLam s expr) (lookup s)
                lookup s = case Map.lookup s bindings of
                               Just expr -> expr
                               Nothing -> error $ "undefined variable " ++ (show s)


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
beta (App x z) = maybe (fmap (App x) (beta z)) (Just . flip App z) (beta x)
beta (Lam y) = fmap Lam (beta y) 

reduce f x = case f x of
                Just x' -> x' : reduce f x'
                Nothing -> []

reductions ((name, expr), desugared) =
    print (expr : desugared : reduced)
  where print xs = concat $ intersperse "\n" $ [name ++ " = " ++ pretty x | x <- xs]
        reduced = take 1 $ reverse $ map (raw []) $ reduce beta (debruijn Map.empty 0 desugared)

main = do 
    str <- getContents
    case parse parseExprs "" str of
        Left err -> print err
        Right xs -> mapM_ (putStrLn . reductions) (zip xs (desugar xs))
