import Text.Printf
import Control.Applicative

{-
3 2 6
[2,2+3], [6,6+3]
=[2,5], [6,8]
=> 1
y1, y2, y3, y4
y3-y2=1

3 1 3
[1,1+3], [3, 3+3]
=[1,4], [3,6]
3,3
=>0
y1, y2, y3, y4
y2>y3
5 10 1
=[5,5+10], [1,1+5]
=[5,15], [1,6]
=> 4
y1,y2,y3,y4
y2>y3 && 
-}

main :: IO ()
main = do
  [w,a,b] <- map toInt . words <$> getLine
  
  print 1
  where
    toInt :: String -> Int
    toInt = read
