import Control.Monad (replicateM_)
main::IO()
main = do
  t <- read <$> getLine :: IO Int
  replicateM_ t solveProblem


solveProblem::IO()
solveProblem = do
  fstLn <- read <$> getLine :: IO (Int,Int)
  n <- fst fstLn
  x <- snd fstLn
  print n
