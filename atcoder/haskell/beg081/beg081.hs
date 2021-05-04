main :: IO ()
main = do
  str <- getLine
  putStrLn $ show $ f str '1'
  where
    f :: String -> Char -> Int
    f [] _ = 0
    f (x:xs) needle = if x == needle
                      then 1 + f xs needle
                      else 0 + f xs needle

