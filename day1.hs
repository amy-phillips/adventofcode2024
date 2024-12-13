import Data.List

split :: [Int] -> ([Int], [Int])
split list =
    case list of
        [] -> ([], [])
        x:xs -> 
            let (evens, odds) = split xs
            in (x:odds, evens)

amyCompare :: ([Int],[Int]) -> [Int]
amyCompare ([],[]) = []
amyCompare (x:xs,y:ys) = x-y : amyCompare (xs,ys)

amySort :: ([Int],[Int]) -> ([Int],[Int])
amySort (x,y) = (sort x, sort y)

amySum :: [Int] -> Int
amySum = foldr ((+) . abs) 0

main :: IO ()
main = do
   putStrLn "Hello, Haskell!" 
   content <- readFile "day1input.txt"
   let numbers = split (map read $ words content :: [Int]) 
   putStrLn ("The input is " ++ show numbers)
   let sorted = amySort numbers 
   putStrLn ("The sorted is " ++ show sorted)
   let zipped = amyCompare sorted
   putStrLn ("The zipped is " ++ show zipped)
   let summed = amySum zipped
   putStrLn ("The summed is " ++ show summed)