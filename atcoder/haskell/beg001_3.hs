import Control.Applicative

main :: IO ()
main = do
  let str = "123"
  print str
  where
    toInt :: String -> Int
    toInt = read

orientation :: Int -> String
orientation deg
  | 1125 <= deg && deg < 3375 = "NNE"
  | deg < 3375 = "NNE"
  | deg < 5625 = "NE"
  | deg < 7875 = "ENE"
  | deg < 10125 = "E"
  | deg < 12375 = "ESE"
  | deg < 14625 = "SE"
  | deg < 16875 = "SSE"
  | deg < 19125 = "S"
  | deg < 21375 = "SSW"
  | deg < 23625 = "SW"
  | deg < 25875 = "WSW"
  | deg < 28125 = "W"
  | deg < 30375 = "WNW"
  | deg < 32625 = "NW"
  | deg < 34875 = "NNW"
  | otherwise = "N"

-- wind dis =

--   where
f 0 = 2
f 1 = 13
-- f n = (f (n - 1) + ((n - 1) * 3)) + (15 * (n - 1) + 3)
f 2 = 15
f n = f (n - 1) + (15 + (n - 2) * 3)
-- solve :: Int -> Int -> (String, Int)
-- solve deg dis = do
--   (orientation deg, )
--   undefined
