module PigLatin (translate) where
twoClusters :: [String]
twoClusters = ["ch", "qu", "th", "rh"]
threeClusters :: [String]
threeClusters = ["squ", "thr", "sch"]
vowelClusters :: [String]
vowelClusters = ["yt", "xr"]
translate :: String -> String
translate = unwords . (map translate') . words
where
translate' xs
| take 3 xs `elem` threeClusters = (drop 3 xs) ++ (take 3 xs) ++ "ay"
| take 2 xs `elem` twoClusters = (drop 2 xs) ++ (take 2 xs) ++ "ay"
| take 2 xs `elem` vowelClusters = xs ++ "ay"
| (head xs) `elem` "aeiou" = xs ++ "ay"
| otherwise = (tail xs) ++ (head xs):"ay"

main = do
  phrase <- getLine
  putStrLn $ translate phrase