import Control.Monad (replicateM)

readWordsLn :: Read a => IO [a]
readWordsLn = map read <$> (words <$> getLine)

readHeadLn :: Read a => IO a
readHeadLn = head <$> readWordsLn

readWordsMultiLn :: Read a => Int -> IO [[a]]
readWordsMultiLn n = replicateM n readWordsLn

main :: IO ()
main = do
  n <- readHeadLn :: IO Int
  chapters <- readWordsMultiLn n :: IO [[Int]]
  page <- readHeadLn :: IO Int
  print (n - solve chapters page)

solve :: [[Int]] -> Int -> Int
solve ((_:x:_):xs) p
  | p > x = solve xs p + 1
  | otherwise = 0
