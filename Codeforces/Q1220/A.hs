import Data.List (intercalate)
import Control.Monad (replicateM_)
main = do
  n <- read <$> getLine :: IO Int
  solveProblem

solveProblem :: IO()
solveProblem = do
  xs <- getLine
  putStrLn $ intercalate " " $ replicate (oneAmount xs) "1" ++ replicate (zeroAmount xs) "0"
  where
    oneAmount = length . filter (== 'n')
    zeroAmount = length . filter (== 'z')
