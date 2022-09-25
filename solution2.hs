isogram :: [Char] -> Bool
Import Data.list
Import Data.char
isIsogram :: String -> Bool
isIsogram =all (== 1)
. map length
. group . sort
. map toLower . filter isLetter
main = do
  text <- getLine
  let val = isogram2 text
      num = if val then "1" else "0"
  putStrLn num
