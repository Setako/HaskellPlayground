import           Control.Arrow

main :: IO ()
main = do
  n <- read <$> getLine :: IO Int
  xs <- (words >>> map read) <$> getLine :: IO [Int]
  print $ min (countOdd xs) (countEven xs)
  where
    countEven = filter (\x -> mod x 2 == 0) >>> length
    countOdd = filter (\x -> mod x 2 == 1) >>> length
