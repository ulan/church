module Parser (parseRawExprs) where
import Control.Applicative hiding ((<|>), many)
import RawExpr
import Text.ParserCombinators.Parsec

rawExprs :: Parser [(String, RawExpr)]
rawExprs = spaces >> separatedBy spaces binding
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

parseRawExprs = parse rawExprs "" 