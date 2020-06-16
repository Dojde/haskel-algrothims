-- https://www.hackerrank.com/challenges/bitter-chocolate/problem

import Control.Monad
import Data.Traversable
import Data.List

getListLine :: IO [Int]
getListLine = do
    line <- getLine
    return (map read $ words line)

checkIfOption :: (Int, Int, Int) -> (Int, Int, Int) -> Bool
checkIfOption (a1, a2, a3) (b1, b2, b3)
    | a3 == b3 && (a1 == b1 || b1 == b2 || a2 == b2) = True
    | a1 == b1 && (a2 == b2 || b2 == b3)             = True
    | otherwise                                      = False


checkBlock :: [(Int, Int, Int)] -> (Int, Int, Int) -> [(Int, Int, Int)]
checkBlock _ (1,0,0) = [(1,0,0)]
checkBlock loses block
    | check = block : loses
    | otherwise = loses
    where check = all (not . checkIfOption block) loses

getLoses :: [(Int, Int, Int)] -> [(Int, Int, Int)]
getLoses l = foldl (checkBlock) [] l

comp :: (Int, Int, Int) -> (Int, Int,Int) -> Ordering
comp (a1, a2, a3) (b1, b2, b3) = compare (a3,a2,a1) (b3,b2,b1)

main :: IO()
main = do
    n <- readLn :: IO Int
    let loses = getLoses . sortBy comp $ [(a1,a2,a3) | a1 <- [1..25], a2 <- [0..a1], a3 <- [0..a2]]
    forM_ [1..n] (\_ -> do
                        cs <- fmap (\[a,b,c] -> (a,b,c)) getListLine
                        putStrLn $ if cs `elem` loses then "LOSE" else "WIN"
                        )
