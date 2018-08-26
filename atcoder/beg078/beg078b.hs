import Control.Applicative

toInt :: String -> Int
toInt = read

solve :: Int -> Int -> Int -> Int -> Int
solve x y z acc =
  if quot x ((y + z) * acc + z) == 1 && mod x ((y + z) * acc + z) < y + z
  then acc
  else solve x y z (acc + 1)

test = do
  putStrLn $ show $ solve 13 3 1 1
  putStrLn $ show $ solve 12 3 1 1
  putStrLn $ show $ solve 100000 1 1 1
  putStrLn $ show $ solve 64146 123 456 1
  putStrLn $ show $ solve 64145 123 456 1

main :: IO ()
main = do
  [x', y', z'] <- map toInt . words <$> getLine
  putStrLn $ show $ solve x' y' z' 1
