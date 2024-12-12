evenSplit :: String -> String -> [String]
evenSplit a b
    | length a == length b = [show(read b :: Int),show (read a :: Int)]
    | otherwise = evenSplit (tail a) (b ++ [head a])

plutonianOutcome :: [String] -> String -> [String]
plutonianOutcome acc cur
    | cur == "0" = acc ++ ["1"]
    | even (length cur) = acc ++ evenSplit cur ""
    | otherwise = acc ++ [show (2024 * read cur)]

blink :: Int -> [String] -> [String] 
blink 1 s = foldl plutonianOutcome [] s
blink i s = foldl plutonianOutcome [] (blink (i-1) s)

main :: IO ()
main = do
    fileInput <- readFile "input.txt"
    let input = words fileInput
    print input
    print $ length (blink 25 input)

-- This is too inefficent but solves it, in ~5 min
-- A better solution is to solve each elem individually then merge lists
