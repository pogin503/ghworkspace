import Control.Applicative
import Data.List
import Control.Monad

toInt :: String -> Int
toInt = read

test1 :: Integer
test1 = div 1 2

test2 :: Integer
test2 = div 4 $ div 1 2

test3 :: Double
test3 = 1900 / 2

-- test4 :: Double
test4 = 1900 * (1 / 2)

-- test1 = div 1 2

-- solve =
   1900 / (/ (fromIntegral 1)  (fromIntegral 2))

main :: IO ()
main = do
  let str = "123"
  print str

{-
Mケース: 1900m 1/2
N-Mケース: 100m
1900×1⁄2+(2×1900)×1⁄4+(3×1900)×1⁄8+…
-}

-- permutation
-- n P n

-- combination
-- n C r
