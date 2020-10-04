module Main where

import Data.List

main :: IO()
main = do
  putStrLn "test"

halve :: [Int] -> [[Int]]
halve lst = [left, right]
  where n = length lst
        left = take (n `div` 2) lst
        right = drop (n `div` 2) lst

safetail :: [a] -> [a]
safetail [] = []
safetail (x:xs) = xs

med :: Ord a => [a] -> a
med lst = (sort lst) !! mid
  where mid = (length lst) `div` 2

qsort :: Ord a => [a] -> [a]
qsort lst | (length lst) <= 1 = lst
          | otherwise = (qsort left) ++ [piv] ++ (qsort right)
  where piv = med lst
        pivDeletedLst = delete piv lst
        left = [x | x <- pivDeletedLst, x < piv]
        right = [x | x <- pivDeletedLst, x >= piv]

data Op = Add
  | Sub
  | Mul
  | Div

data Expr = Val Int | App Op Expr Expr
