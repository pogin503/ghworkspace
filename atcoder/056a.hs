import Text.Printf
import Control.Applicative

main :: IO ()
main = do
  [a,b] <- words <$> getLine
  -- (maxCount, minCount) <-
  -- printf "%d %d\n" (maximum countList) (minimum countList)
  if a == "H"
  then if b == "H"
       then putStrLn "H"
       else putStrLn "D"
  else if b == "H"
       then putStrLn "D"
       else putStrLn "H"

