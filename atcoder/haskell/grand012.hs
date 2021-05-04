import Control.Applicative
import Control.Monad

-- solve :: [Int] -> Int
solve xs = xs1 xs
  where
    xs1 [] = []
    xs1 xs = [take 3 xs] ++ xs1 $ drop 3 xs

main = do
  n <- readLn :: IO Int
  xs <- map toInt . words <$> getLine
  -- let x = solve xs
  print $ solve xs
  where
    toInt :: String -> Int
    toInt = read
