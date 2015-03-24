module Main where

import qualified Data.List as L

main :: IO ()
main = do
    putStrLn "First dimension?"
    n <- getLine
    let n' = read n :: Int

    putStrLn "Second dimension?"
    m <- getLine
    let m' = read m :: Int

    putStrLn "Starting square?"
    s <- getLine
    let s' = parseSquare s

    validate n' m' s'

    print $ tour n' m' s'


type Square = (Int, Int)


parseSquare :: String -> Square
parseSquare s = (read $ res !! 0, read $ res !! 1)
    where res = words s


validate :: Int -> Int -> Square -> IO ()
validate n m (x, y)
    | x < 0 || n < x    = error "Invalid x-coordinate"
    | y < 0 || m < y    = error "Invalid y-coordinate"
    | otherwise         = return ()


-- We'll use Warnsdorff's Rule (https://en.wikipedia.org/wiki/Knight%27s_tour) with
-- random tie-breaking. This means that for suitably large boards, the algorithm
-- may fail to find a tour even when a tour exists.
tour :: Int -> Int -> Square -> Maybe [Square]
tour n m s = tour' [(x, y) | x <- [0..n], y <- [0..m]] [] s


tour' :: [Square] -> [Square] -> Square -> Maybe [Square]
tour' [] t s = Just (t ++ [s])
tour' remaining t s = do
    next <- nextSquare remaining t s
    tour' (filter (/= next) remaining) (t ++ [s]) next


nextSquare :: [Square] -> [Square] -> Square -> Maybe Square
nextSquare rs t s = Nothing
