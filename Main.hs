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
parseSquare s = (read $ head res, read $ last res)
    where res = words s


validate :: Int -> Int -> Square -> IO ()
validate n m (x, y)
    | x < 0 || n < x    = error "Invalid x-coordinate"
    | y < 0 || m < y    = error "Invalid y-coordinate"
    | otherwise         = return ()


-- We'll use Warnsdorff's Rule (https://en.wikipedia.org/wiki/Knight%27s_tour) with
-- undefined tie-breaking. This means that for suitably large boards, the algorithm
-- may fail to find a tour even when a tour exists.
tour :: Int -> Int -> Square -> Maybe [Square]
tour n m s = tour' (filter (/= s) [(x, y) | x <- [1..n], y <- [1..m]]) [] s


tour' :: [Square] -> [Square] -> Square -> Maybe [Square]
tour' [] t s = Just (t ++ [s])
tour' remaining t s = do
    next <- nextSquare remaining t s
    tour' (filter (/= next) remaining) (t ++ [s]) next


nextSquare :: [Square] -> [Square] -> Square -> Maybe Square
nextSquare rs t s
    | null nexts        = Nothing
    | otherwise         = Just . snd . minimum $
                            [(length (surrounding next rs), next) | next <- nexts]
        where nexts = surrounding s rs


surrounding :: Square -> [Square] -> [Square]
surrounding (x, y) rs = filter (`elem` rs) [ (x + 1, y + 2), (x + 2, y + 1)
                                           , (x - 1, y + 2), (x + 2, y - 1)
                                           , (x - 1, y - 2), (x - 2, y - 1)
                                           , (x + 1, y - 2), (x - 2, y + 1)]
