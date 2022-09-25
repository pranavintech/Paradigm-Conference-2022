module Spiral (spiral) where
import Data.List (transpose)
spiral :: Int -> [[Int]]
spiral n = spiral' 1 n n
spiral' :: Int -> Int -> Int -> [[Int]]
spiral' _ 0 _ = []
spiral' s h w = [s..s + w - 1] : roll (spiral' (s + w) w (h - 1))
    where roll = (map reverse) . transpose
