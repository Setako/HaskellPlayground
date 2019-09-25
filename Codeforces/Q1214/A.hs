main = do
  remain <- read <$> getLine :: IO Int
  dollar <- read <$> getLine :: IO Int
  euro <- read <$> getLine :: IO Int
  print $ findBest dollar (euro * 5) 1750

findBest :: Int -> Int -> Int -> Int
findBest dollar euro remain
  | remain < dollar && remain < euro = buyNothing
  | remain < dollar && remain >= euro = buyEuro
  | remain < euro && remain >= dollar = buyDollar
  | otherwise =
    if buyEuro < buyDollar
      then buyEuro
      else buyDollar
  where
    buyEuro = findBest dollar euro (remain - euro)
    buyDollar = findBest dollar euro (remain - dollar)
    buyNothing = remain
