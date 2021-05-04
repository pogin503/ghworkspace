toInt :: String -> Int
toInt = read

main :: IO ()
main = do
  [a, b] <- map toInt . words <$> getLine
  if even (a * b)
    then putStrLn "Even"
    else putStrLn "Odd"
