import           Control.Monad (replicateM)

readWordsLn :: Read a => IO [a]
readWordsLn = map read <$> (words <$> getLine)

readHeadLn :: Read a => IO a
readHeadLn = head <$> readWordsLn

readWordsMultiLn :: Read a => Int -> IO [[a]]
readWordsMultiLn n = replicateM n readWordsLn

main::IO()
main = do
  [n,m] <- readWordsLn :: IO [Int]
  a <- readWordsMultiLn n :: IO (Matrix Int)
  b <- readWordsMultiLn n :: IO (Matrix Int)
  print $ solve n m a b 0 0

solve:: Int -> Int -> Matrix Int -> Matrix Int -> Int -> Int -> Bool
solve n m a b x y
  | y == n = True
  | x == m = solve n m a b 0 (y+1)
--  | otherwise =


tryTranspose:: Int -> Int -> Matrix Int -> Matrix Int -> Int -> Int -> Int -> Int
tryTranspose n m a b x y chk
  | y+chk == n || x+chk == m = chk -1
  | a!!y!!(x+chk) == b !!(y+chk)!!x && a!!(y+chk)!!x == b!!y!!(x+chk) = tryTranspose n m a b x y (chk+1)
  | a!!y!!(x+chk) == b!!y!!(x+chk) && a!!(y+chk)!!x == b!!(y+chk)!!x = chk
  | otherwise = chk-1

type Matrix a = [[a]]

--Matrix

subMatrix :: Matrix a    -- ^ The origin matrix
          -> (Int,Int)  -- ^ The start point, col index and row index
          -> (Int,Int)   -- ^ The end point, col index and row index
          -> Matrix a
subMatrix m s e=  (\xs -> drop (fst e) . take row $ xs) <$> (drop (fst s) . take col $ m)
  where col = fst e - fst s
        row = snd e - snd s

insertMatrix :: Matrix a   -- ^ The origin matrix
             -> Matrix a   -- ^ The insertingMatrix
             -> (Int,Int)  -- ^ The target position, col index and row index
             -> Matrix a

insertMatrix m1 m2 p =  r1 ++ r2 ++ r3
  where r1 =take (fst p) m1
        r2m1 =(drop (fst p) . take (length m2) $ m1)
        r2 = insertAtSameHeightMatrix r2m1 m2 (snd p)
        r3 = drop (fst p + length m2) m1
        insertAtSameHeightMatrix (x:xs) (y:ys) p = (take p x ++ y ++ drop (p + length y) x):insertAtSameHeightMatrix xs ys p
