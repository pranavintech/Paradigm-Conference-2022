Module hamming (distance) where
hammingDistance :: [Char] -> [Char] -> Int
distance :: String -> String -> Maybe Int
distance xs ys
| length xs /= length ys = Nothing
| otherwise = Just . sum . map (\(x, y) -> if x == y then 0 else 1) $ zip xs ys
main = do
  text <- getLine
  let phrases = splitOn " " text
      a = head phrases
      b = head $ tail phrases
  putStrLn $ show $ hammingDistance a b
