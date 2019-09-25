main = do
  b <- read <$> getLine :: IO Int
  g <- read <$> getLine :: IO Int
  n <- read <$> getLine :: IO Int
  print $ solve b g n

solve :: Int -> Int -> Int -> Int
solve b g n = left + right + 1
  where
    avg = n `div` 2
    left = max 0 $ b - n
    right = max 0 $ g - n
