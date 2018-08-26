
import Control.Monad
import Control.Applicative

main :: IO ()
main = do
  [a,b,c] <- map toInt . words <$> getLine
  if (b - a) == (c - b) 
  then putStrLn "YES"
  else putStrLn "NO"
  where
    toInt :: String -> Int
    toInt = read
