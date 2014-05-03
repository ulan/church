module Util where
import Data.List

reduce rule initial = case rule initial of
  Just x -> x : reduce rule x
  Nothing -> []

report name steps =
  concat $ intersperse "\n" $ [name ++ " = " ++ x | x <- steps]
