module Lib
    (solve
    , ceasarDecode
    , ceasarEncode
    ) where

import Data.List (elemIndex)
import Data.Maybe (fromJust)
import Text.Regex.PCRE

ceasarEncode :: String -> Int -> String -> String -> String
ceasarEncode str n charset allowedStrRx =
  map (\c -> if [c] =~ allowedStrRx
             then encode c n
             else c) str
  where
    encode :: Char -> Int -> Char
    encode c n' =
      charset !! (mod ((fromJust $ elemIndex c charset) + n') $ length charset)

ceasarDecode :: String -> Int -> String -> String -> String
ceasarDecode str n charset allowedStrRx =
  map (\c -> if [c] =~ allowedStrRx
             then decode c n
             else c) str
  where
    decode :: Char -> Int ->  Char
    decode c n' =
      charset !! (mod ((fromJust $ elemIndex c charset) - n') $ length charset)

solve :: String -> [String]
solve q = do
  decodedList <- map (\n -> ceasarDecode q n charset allowedStrRx) [1..(length charset)]
  filter (=~ "person") [decodedList]
  where
    charset :: String
    charset = "abcdefghijklmnopqrstuvwxyz .,-"
    allowedStrRx :: String
    allowedStrRx = "[abcdefghijklmnopqrstuvwxyz .,-]"
