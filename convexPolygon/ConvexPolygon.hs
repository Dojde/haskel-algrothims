import Text.Printf
import Control.Monad
import Data.List
import Data.Tuple
import Data.Ord

-- This is a subproblem to convex hull.

getListLine :: IO [Int]
getListLine = do
    line <- getLine
    return (map read $ words line)

getCos :: (Int, Int) -> (Int, Int) -> Double
getCos a b = dotProd / absSum
    where
            fstNbr    = (fst b - fst a)
            sndNbr    = (snd b - snd a)
            dotProd   = fromIntegral fstNbr
            absSum    = sqrt . fromIntegral $ (fstNbr*fstNbr + sndNbr*sndNbr)

getDistance :: (Int, Int) -> (Int, Int) -> Double
getDistance (x1,y1) (x2,y2) = sqrt . fromIntegral $ (x'*x' + y'*y')
    where
        x' = x1-x2
        y' = y1-y2

getAnchorIndex :: [(Int, Int)] -> Int
getAnchorIndex = fst . minimumBy (comparing (swap . snd)) . zip [0..]

getAnchorList :: [(Int, Int)] -> Int ->((Int, Int), [(Int, Int)])
getAnchorList xs n = ((xs!!n), p1 ++ (tail p2))
    where (p1, p2) = splitAt n xs


anchorSort :: [(Int, Int)] -> ((Int, Int), [(Int, Int)])
anchorSort cs = (anchor, sorted_cs)
    where
        (anchor, l) = getAnchorList cs . getAnchorIndex $ cs -- Get anchor
        comp        = mconcat   [  comparing (Down . getCos anchor)
                                ,  comparing (getDistance anchor) ]
        sorted_cs   = sortBy comp l

leftTurn :: (Int, Int) -> (Int, Int) -> (Int, Int) -> Bool
leftTurn (x1,y1) (x2, y2) (x3, y3) = direction >= 0
    where
        direction = (x2-x1) * (y3-y1) - (y2-y1) * (x3-x1)

traversePoints :: [(Int, Int)] -> Bool
traversePoints cs = False `elem` [ leftTurn a b c | (a,b,c) <- zip3 cs (tail cs) (tail . tail $ cs)]

main :: IO()
main = do
    n   <- readLn :: IO Int -- Read input
    cs  <- forM [1..n] (\_ -> fmap (\[a, b] -> (a, b)) getListLine) -- Read input
    let (anchor, sorted_cs) = anchorSort cs
    print $ if (traversePoints $ anchor : sorted_cs)
        then "YES"
        else "NO"
