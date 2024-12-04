import Text.Regex.TDFA((=~), getAllTextMatches)

validMemoryFragmentsRegex = "mul\\([0-9]{1,3},[0-9]{1,3}\\)"
validMemoryFragmentsRegexWithDoDont = "mul\\([0-9]{1,3},[0-9]{1,3}\\)|do\\(\\)|don't\\(\\)"
multiplicandCaptureGroupRegex = "mul\\(([0-9]{1,3}),([0-9]{1,3})\\)"

getValidMemoryFragments :: String -> [String]
getValidMemoryFragments mem = 
    getAllTextMatches(mem =~ validMemoryFragmentsRegex) :: [String]

getValidMemoryFragmentsWithDoDont :: String -> [String]
getValidMemoryFragmentsWithDoDont mem = 
    getAllTextMatches(mem =~ validMemoryFragmentsRegexWithDoDont) :: [String]

extractMultiplicands :: String -> (Int, Int)
extractMultiplicands s = do
    let (_, _, _, ints)= s =~ multiplicandCaptureGroupRegex :: (String, String, String, [String])
    (read(last ints), read(head ints))

getSumOfProducts :: [(Int, Int)] -> Int
getSumOfProducts multiplicandArray =
    foldl (\acc cur -> acc + uncurry (*) cur) 0 multiplicandArray

recursiveDoDontSum :: [String] -> Int -> Bool -> Int
recursiveDoDontSum sArr total shouldInclude =
    if null sArr
        then
            total
        else
            case head sArr of 
                "do()" -> recursiveDoDontSum (tail sArr) total True
                "don't()" -> recursiveDoDontSum (tail sArr) total False
                _ -> if shouldInclude 
                    then recursiveDoDontSum (tail sArr) (total + getSumOfProducts([extractMultiplicands(head sArr)])) shouldInclude
                    else recursiveDoDontSum (tail sArr) total shouldInclude

processInputLine :: String -> Int
processInputLine s = getSumOfProducts . map extractMultiplicands . getValidMemoryFragments $ s

processInputLine2 :: String -> Int
processInputLine2 s = recursiveDoDontSum (getValidMemoryFragmentsWithDoDont s) 0 True

main :: IO ()
main = do
    fileContent <- readFile "./example2.txt"
    let inputLines = lines fileContent
    print $ "Solution to part one:  " ++ show(sum(map processInputLine inputLines))
    print $ "Solution to part two:  " ++ show(sum(map processInputLine2 inputLines))
