module Main where


import qualified Data.List as L
import Graphics.GD
import Foreign.C.Types
import Control.Applicative


-- used for image generation
sqSize :: Int
sqSize = 80

border :: Int
border = 5


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

    putStrLn "Output file name (blank for none)?"
    fname <- getLine

    validate n' m' s'
    let t = tour n' m' s'

    drawTour t fname

    print t


type Square = (Int, Int)


parseSquare :: String -> Square
parseSquare s = (read $ head res, read $ last res)
    where res = words s


validate :: Int -> Int -> Square -> IO ()
validate n m (x, y)
    | x <= 0 || n < x = error "Invalid x-coordinate"
    | y <= 0 || m < y = error "Invalid y-coordinate"
    | otherwise      = return ()


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
    | null nexts = Nothing
    | otherwise  = Just . snd . minimum $ [(length (surrounding next rs), next) | next <- nexts]
        where nexts = surrounding s rs


surrounding :: Square -> [Square] -> [Square]
surrounding (x, y) rs = filter (`elem` rs) [ (x + 1, y + 2), (x + 2, y + 1)
                                           , (x - 1, y + 2), (x + 2, y - 1)
                                           , (x - 1, y - 2), (x - 2, y - 1)
                                           , (x + 1, y - 2), (x - 2, y + 1)
                                           ]


drawTour :: Maybe [Square] -> String -> IO ()
drawTour Nothing _ = return ()
drawTour _ "" = return ()
drawTour (Just sqs) fname = do
    image <- let (x, y) = maximum sqs in newImage (x * sqSize + 2 * border, y * sqSize + 2 * border)
    fillImage (rgb 0 0 0) image 
    (sequence_ . getZipList) $ drawFilledRectangle <$> ZipList (map upLeft sqs) <*> ZipList (map lowRight sqs)
            <*> ZipList (map getColor sqs) <*> ZipList (repeat image)
    drawFilledRectangle (upLeft . head $ sqs) (lowRight . head $ sqs) (rgb 0 200 0) image
    drawFilledRectangle (upLeft . last $ sqs) (lowRight . last $ sqs) (rgb 200 0 0) image
    (sequence_ . getZipList) $ drawLine <$> ZipList (map getCenter sqs) <*> ZipList (map getCenter (tail sqs))
                                        <*> ZipList (repeat (rgb 0 0 0)) <*> ZipList (repeat image)
    savePngFile (fname ++ ".png") image


upLeft :: Square -> Point
upLeft (x, y) = (border + (x - 1)*sqSize, border + (y - 1)*sqSize)


lowRight :: Square -> Point
lowRight (x, y) = (border + x*sqSize, border + y*sqSize)


getColor :: Square -> Color
getColor (x, y)
    | even (x + y)  = rgb 100 100 100
    | otherwise     = rgb 200 200 200


getCenter :: Square -> Point
getCenter sq = let (x1, y1) = upLeft sq in (x1 + sqSize `div` 2, y1 + sqSize `div` 2)
