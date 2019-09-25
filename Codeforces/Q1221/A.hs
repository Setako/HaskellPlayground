import           Control.Monad (replicateM_)

main = do
  q <- read <$> getLine :: IO Int
  replicateM_ q (readQuery >>= ((putStrLn . (\x -> if x then "YES" else "No")) . solve))

readQuery :: IO [Int]
readQuery = do
  n <- read <$> getLine :: IO Int
  map read . words <$> getLine :: IO [Int]

solve :: [Int] -> Bool
solve = findAmount 2048 1
  where
    findAmount required amount xs
      | length (filter (== required) xs) >= amount = True
      | required == 1 = False
      | otherwise = findAmount (required `div` 2) ((amount - length (filter (== required) xs)) * 2) xs
