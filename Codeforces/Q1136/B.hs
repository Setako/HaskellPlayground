import           Control.Monad (replicateM)

readWordsLn :: Read a => IO [a]
readWordsLn = map read <$> (words <$> getLine)

readHeadLn :: Read a => IO a
readHeadLn = head <$> readWordsLn

readWordsMultiLn :: Read a => Int -> IO [[a]]
readWordsMultiLn n = replicateM n readWordsLn

main :: IO ()
main = do
  [k, n] <- readWordsLn :: IO [Int]
  print (minimum [n - 1, k - n] + k + k - 1 + k + 1)
