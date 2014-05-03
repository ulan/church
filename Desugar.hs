module Desugar where
import RawExpr
import qualified Data.Set as Set
import qualified Data.Map as Map

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
