module Main where

import           Control.Concurrent
import           Data.List


halve :: [Int] -> [[Int]]
halve lst = [left, right]
  where n = length lst
        left = take (n `div` 2) lst
        right = drop (n `div` 2) lst

safetail :: [a] -> [a]
safetail []       = []
safetail (_ : xs) = xs

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

-- 配列が対象かどうか。
seqIsSymmetry :: Ord a => [a] -> Bool
seqIsSymmetry seq = left == (reverse right)
  where piv = (length seq) `div` 2
        left = take piv seq
        right = if (odd (length seq))
                then drop (piv + 1) seq
                else drop piv seq

width :: Int
width = 5

height :: Int
height = 5

type Pos = (Int, Int)

type Board = [Pos]

glider:: Board
glider = [(4,2),(2,3),(4,3),(3,4),(4,4)]

goto :: Pos -> IO()
goto (x, y) =
  putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

seqn :: [IO a] -> IO ()
seqn [] = return ()
seqn (a:as) = do a
                 seqn as

writeat :: Pos -> String -> IO ()
writeat p xs = do goto p
                  putStr xs

showcells :: Board -> IO ()
showcells b = seqn [writeat p "." | p <- b]

isAlive :: Board -> Pos -> Bool
isAlive b p = elem p b

isEmpty :: Board -> Pos -> Bool
isEmpty b p = not (isAlive b p)

wrap :: Pos -> Pos
wrap (x, y) = (((x - 1) `mod` width) + 1,
               ((y - 1) `mod` height) + 1)

neighbs :: Pos -> [Pos]
neighbs (x, y) = map wrap [(a, b) | a <- [x-1..x+1], b <- [y-1..y+1], not (x == a && y == b)]

liveneighbs :: Board -> Pos -> Int
liveneighbs b = length . filter (isAlive b) . neighbs

survivors :: Board -> [Pos]
survivors b = [p | p <- b, elem (liveneighbs b p) [2, 3]]

births :: Board -> [Pos]
births b = [(x, y) | x <- [1..width],
           y <- [1..height],
           isEmpty b (x, y),
           liveneighbs b (x, y) == 3]

nextgen :: Board -> Board
nextgen b = survivors b ++ births b

cls :: IO()
cls = putStr "\ESC[2J"

wait :: Int -> IO()
wait n = seqn [return () | _ <- [1..n]]

life :: Board -> IO()
life b = do cls
            showcells b
            threadDelay 3000
            life (nextgen b)

main :: IO()
main = do
  putStrLn "Start!"
  life glider
