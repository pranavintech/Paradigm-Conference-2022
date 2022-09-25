encrypt :: [Char] -> [Char] -> [Char]
import Data.Char
-- exercise chapter 11encode :: Char -> Intencode x = ord x - ord 'A'
decode :: Int -> Chardecode x = chr (x + ord 'A')
shift :: (Int -> Int -> Int) -> Int -> Char -> Charshift f x ch =  decode $ f (encode ch) x `mod` 26
rightShift :: Int -> Char -> CharrightShift = shift (+)
leftShift :: Int -> Char -> CharleftShift = shift (-)
encodeString :: String -> [Int]encodeString str = map encode str
type Secret = Stringtype PlainText = Stringtype CipherText = String
vignereString :: Secret -> PlainText -> StringvignereString secret plain = take len $ cycle secret
                          where len = length $ concat $ words plain
vignereCode :: Secret -> PlainText -> [Int]vignereCode secret plain = encodeString $ vignereString secret plain

vignere :: Secret -> PlainText -> CipherTextvignere secret plain =
  zipWith rightShift code plainNoSpace
  where code = vignereCode secret plain
        plainNoSpace = concat $ words plain
unvignere :: Secret -> CipherText -> PlainTextunvignere secret cipher =
  zipWith leftShift code cipherNoSpace
  where code = vignereCode secret cipher
        cipherNoSpace = concat $ words cipher

main = do
  word <- getLine
  key  <- getLine
  putStrLn $ encrypt word key
