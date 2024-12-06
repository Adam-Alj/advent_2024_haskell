
getIntTuple :: (String, String) -> (Int, Int)
getIntTuple (a, b) = (read a, read b)

stringRuleTuple :: String -> (String, String)
stringRuleTuple s =
    (fst (span (/= '|') s), tail (snd (span (/= '|') s)))

ruleToTuple :: String -> (Int, Int)
ruleToTuple = getIntTuple . stringRuleTuple

mergeRuleToProcessedRule :: (Int, Int) -> ([Int], Int, [Int]) -> ([Int], Int, [Int])
mergeRuleToProcessedRule (before, after) (bList, cur, aList)
  | cur == before = (bList, cur, after:aList)
  | cur == after = (before:bList, cur, aList)
  | otherwise = (bList, cur, aList)

poplulateWithMissingValue :: Int -> [([Int], Int, [Int])] ->[([Int], Int, [Int])]
poplulateWithMissingValue i accRules =
    if i `notElem` map (\(_,c,_) -> c) accRules then ([], i, []):accRules else accRules

poplulateWithMissingValues :: (Int, Int) -> [([Int], Int, [Int])] ->[([Int], Int, [Int])]
poplulateWithMissingValues (a, b) accRules =
    poplulateWithMissingValue a (poplulateWithMissingValue b accRules)

insertRule :: (Int, Int) -> [([Int], Int, [Int])] ->[([Int], Int, [Int])]
insertRule rule accRules =
    map (mergeRuleToProcessedRule rule) (poplulateWithMissingValues rule accRules)

processedRules :: [(Int, Int)] -> [([Int], Int, [Int])] -> [([Int], Int, [Int])]
processedRules rawRules accProcessRules =
    if null rawRules
        then accProcessRules
        else processedRules (tail rawRules) (insertRule (head rawRules) accProcessRules)

splitOnComma :: String -> [String]
splitOnComma [] = [""]
splitOnComma (c:cs)
    | c == ','  = "" : rest
    | otherwise = (c : head rest) : tail rest
  where
    rest = splitOnComma cs

compareToRule :: [Int] -> [Int] -> Bool
compareToRule c rule = True

validAgainstRule :: [Int] -> [Int] -> ([Int], Int, [Int]) -> Bool
validAgainstRule before after rules = True

isValidOrder :: [Int] -> [([Int], Int, [Int])] -> Bool
isValidOrder r rules = True

main :: IO ()
main = do
    rulesFileContent <- readFile "exampleRules.txt"
    let rules = processedRules (map ruleToTuple (lines rulesFileContent)) []
    print rules
    inputFileContent <- readFile "exampleInput.txt"
    let a = map splitOnComma (lines inputFileContent)
    let input = map (map (\z -> read z :: Int)) a
    print input