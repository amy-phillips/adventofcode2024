import Data.List

split :: [Int] -> ([Int], [Int])
split list =
    case list of
        [] -> ([], [])
        x:xs -> 
            let (evens, odds) = split xs
            in (x:odds, evens)

compareLists :: ([Int],[Int]) -> [Int]
compareLists ([],[]) = []
compareLists (x:xs,y:ys) = x-y : compareLists (xs,ys)

sortTwoLists :: ([Int],[Int]) -> ([Int],[Int])
sortTwoLists (x,y) = (sort x, sort y)

absSum :: [Int] -> Int
absSum = foldr ((+) . abs) 0

main :: IO ()
main = do
   putStrLn "Hello, Haskell!" 
   content <- readFile "day1input.txt"
   let numbers = split (map read $ words content :: [Int]) 
   putStrLn ("The input is " ++ show numbers)
   let sorted = sortTwoLists numbers 
   putStrLn ("The sorted is " ++ show sorted)
   let zipped = compareLists sorted
   putStrLn ("The zipped is " ++ show zipped)
   let summed = absSum zipped
   putStrLn ("The summed is " ++ show summed)