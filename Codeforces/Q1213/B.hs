import           Control.Arrow
import           Control.Monad (replicateM)
import           Prelude

main :: IO ()
main = do
  n <- read <$> getLine :: IO Int
  x <- replicateM n solve
  return ()

solve :: IO ()
solve = do
  n <- read <$> getLine :: IO Int
  xs <- (words >>> map read) <$> getLine :: IO [Int]
  print $ findSmall xs

findSmall :: [Int] -> Int
findSmall = reverse >>> findSmallWalk 1000001
  where
    findSmallWalk m [] = 0
    findSmallWalk m (y:ys) = findSmallWalk (min y m) ys +  if y > m then 1 else 0
