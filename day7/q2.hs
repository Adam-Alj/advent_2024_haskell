splitInputLine :: String -> (String, String)
splitInputLine s =
    splitAt (length (takeWhile (/= ':') s)) s

processInputLine :: String -> (Int, [Int])
processInputLine s = do
    let (testVal, rest) = splitInputLine s
    (read testVal, map read (words (tail rest)))

lineValid :: (Int, [Int]) -> Bool
lineValid (target, numbers) =
    isEquationPossiblyValid target 0 numbers (+) 
    || isEquationPossiblyValid target 1 numbers (*)
    || isEquationPossiblyValid target (head numbers) (tail numbers) integerConcat

integerConcat :: Int -> Int -> Int
integerConcat a b =
    read (show a ++ show b)

isEquationPossiblyValid :: Int -> Int -> [Int] -> (Int -> Int -> Int) -> Bool
isEquationPossiblyValid target total numbers operation
    | null numbers = target == total
    | total > target = False
    | otherwise = 
        isEquationPossiblyValid target (total `operation` head numbers) (tail numbers) (+) 
        || isEquationPossiblyValid target (total `operation` head numbers) (tail numbers) (*)
        || isEquationPossiblyValid target (total `operation` head numbers) (tail numbers) integerConcat

main :: IO ()
main = do
    fileInput <- readFile "input.txt"
    let inputLines = map processInputLine (lines fileInput)
    print $ foldl (\acc (_,a) -> acc + a) 0 (filter fst (zip (map lineValid inputLines) (map fst inputLines)))

-- works for q2, such a slow brute force solution though. Took like 30s to run on input with my 14900K lol