import Control.Applicative

convert :: String -> Int
convert x =
  case x of
    "A" -> 10
    "B" -> 11
    "C" -> 12
    "D" -> 13
    "E" -> 14
    "F" -> 15
    _   -> error "A-F"

main :: IO ()
main = do
  [x,y] <- map convert . words <$> getLine
  putStrLn $ solve x y
  where
    solve x' y'
     | x' > y' = ">"
     | x' < y' = "<"
     | otherwise = "="
