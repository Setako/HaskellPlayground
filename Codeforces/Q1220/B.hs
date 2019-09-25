import           Control.Monad (replicateM)
import           Data.List     (maximumBy, minimumBy)
import           Data.Ord      (comparing)

main = do
  n <- read <$> getLine :: IO Int
  xss <- replicateM n $ map read . words <$> getLine
  putStrLn $ unwords $ map show $ solve xss

solve :: Integral a => [[a]] -> [a]
solve xss = map (\x -> x `div` (round . sqrt . fromIntegral) (raw !! 0)) raw
  where
    baseRow = xss !! 0
    ref = xss !! 1
    scale = fromIntegral (ref !! 2) / fromIntegral (baseRow !! 2)
    raw = round (fromIntegral (ref !! 0) / scale) : drop 1 baseRow
