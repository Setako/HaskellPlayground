import           Control.Arrow ((>>>))
import           Control.Monad (replicateM_)

main :: IO ()
main = do
  n <- read <$> getLine
  replicateM_ n readIn

readIn :: IO ()
readIn = do
  xs <- (words >>> map read) <$> getLine
  print (solve xs)

solve :: [Int] -> Int
solve (str:int:exp:_) =
  if offset + exp <= 0
    then 0
    else ceiling (fromIntegral (offset + exp) / 2)
  where
    offset = str - int
solve _ = 0
