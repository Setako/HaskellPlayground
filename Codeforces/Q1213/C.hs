import           Control.Arrow ((>>>))
import           Control.Monad (liftM2, replicateM, replicateM_)

main :: IO ()
main = do
  n <- read <$> getLine :: IO Int
  replicateM_ n solveCase

solveCase :: IO ()
solveCase = do
  line <- getLine
  print $ solve ((words >>> map read >>> head) line) ((words >>> map read >>> last) line)

solve :: Integer -> Integer -> Integer
solve n m = quot n cycleSum * cycleValue + runCycle m (quot (mod n cycleSum) m) 1
  where
    cycleSum = m * 10
    cycleValue = runCycle m 10 1

runCycle :: Integer -> Integer -> Integer -> Integer
runCycle m times startAt
  | times == 0 = 0
  | otherwise = runCycle m (times - 1) (startAt + 1) + (show >>> last >>> (: []) >>> read) (m * startAt)
