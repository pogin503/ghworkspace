import Control.Monad
import Control.Applicative

solve ev od = solve' ev od
  where
    solve' :: String -> String -> String
    solve' [] [] = []
    solve' (x:xs) [] = [x]
    solve' (x:xs) (y:ys) = x : y : solve' xs ys

main :: IO ()
main = do
  ev <- getLine
  od <- getLine
  putStrLn $ solve ev od
