module Main where
import qualified Data.Set as Set
import qualified Data.Map as Map
import Text.ParserCombinators.Parsec
import Data.List

data Expr = Var String
          | App Expr Expr
          | Lam String Expr
    deriving (Eq, Ord, Show)

pretty :: Expr -> String
pretty (Var s) = s
pretty (App x (y@(App _ _))) = pretty x ++ " (" ++ pretty y ++ ")"
pretty (App x y) = pretty x ++ " " ++ pretty y
pretty (Lam x y) = "(\\" ++ x ++ " -> " ++ pretty y ++ ")"

parseExprs :: Parser [(String, Expr)]
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
        var = identifier >>= return . Var
        apply [x] = x
        apply (x : xs) = foldl App x xs
        abstract vars body = foldr Lam body vars 
        identifier = many1 (letter <|> digit <|> oneOf "_$'")
        inlineSpaces = many (oneOf " \t") >> return ()
        separatedBy sep item = many1 (item >>= \x -> sep >> return x)

freeVars :: Expr -> Set.Set String
freeVars (v@(Var s)) = Set.singleton s
freeVars (App x y) = Set.union (freeVars x) (freeVars y)
freeVars (Lam x y) = Set.delete x (freeVars y)

fresh :: Set.Set String -> String
fresh forbidden = head [x | x <- generate, not (Set.member x forbidden)]
  where generate = [x : xs | xs <- ("":generate), x <- alphabet]
        alphabet = ['a' .. 'z']


subst :: String -> Expr -> Expr -> Expr
subst var new (Var s) | var == s = new
                      | otherwise = (Var s)
subst var new (App x y) = App (subst var new x) (subst var new y)
subst var new (Lam x y) | var == x = Lam x y
                        | Set.member x freenew = Lam x' (subst var new y') 
                        | otherwise = Lam x (subst var new y) 
  where freenew = freeVars new
        x' = fresh (Set.union freenew (freeVars y))
        y' = subst x (Var x') y

desugar :: [(String, Expr)] -> [Expr]
desugar = reverse . snd . foldl desugarOne (Map.empty, [])
  where desugarOne (bindings, result) (x, y) = (bindings', result')
          where bindings' = Map.insert x y' bindings
                result' = y' : result
                y' = foldl abstract y (Set.toList $ freeVars y)
                abstract expr s = App (Lam s expr) (lookup s)
                lookup s = case Map.lookup s bindings of
                               Just expr -> expr
                               Nothing -> error $ "undefined variable " ++ (show s)
beta :: Expr -> Maybe Expr
beta (Var _) = Nothing
beta (App (Lam x y) z) = Just $ subst x z y
beta (App x z) = fmap (flip App z) (beta x)
beta (Lam x y) = Nothing 

reduce f x = case f x of
                Just x' -> x' : reduce f x'
                Nothing -> []

reductions ((name, expr), desugared) =
    print (expr : desugared :reduce beta desugared)
  where print xs = concat $ intersperse "\n" $ [name ++ " = " ++ pretty x | x <- rs]
        rs = desugared : (take 1 $ reverse $ reduce beta desugared)

main = do 
    str <- getContents
    case parse parseExprs "" str of
        Left err -> print err
        Right xs -> mapM_ (putStrLn . reductions) (zip xs (desugar xs))
