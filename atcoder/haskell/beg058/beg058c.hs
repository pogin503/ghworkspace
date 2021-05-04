-- {-# FlexibleContents #-}
import Control.Monad
import Control.Applicative
import Data.List (nub)
import qualified Data.Map as Map

solve :: Int -> [String] -> String
solve n xs = do
  xs2 <- nub $ concat xs
  xs1 <- map (\x -> if length x > n
                    then "1"
                    else "2") xs
  xs1
  where
    f :: String -> [String]
    f xs'
      | length xs' < n = []
      | otherwise = take n xs' : f (drop n xs')

main :: IO ()
main = do
  n <- toInt <$> getLine
  xs <- lines <$> getContents

  putStrLn "YES"
  where
    toInt :: String -> Int
    toInt = read
