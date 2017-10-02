module Lib
    (solve
    , ceasarDecode
    , ceasarEncode
    ) where

import Data.List (elemIndex)
import Data.Maybe (fromJust)
import Text.Regex.PCRE

ceasarEncode :: String -> Int -> String -> String -> String
ceasarEncode cipher seed charset allowedStrRx =
  map (\c -> if [c] =~ allowedStrRx
             then encode c seed
             else c) cipher
  where
    encode :: Char -> Int -> Char
    encode c seed' =
      charset !! (mod ((fromJust $ elemIndex c charset) + seed') $ length charset)

ceasarDecode :: String -> Int -> String -> String -> String
ceasarDecode plain seed charset allowedStrRx =
  map (\c -> if [c] =~ allowedStrRx
             then decode c seed
             else c) plain
  where
    decode :: Char -> Int ->  Char
    decode c seed' =
      charset !! (mod ((fromJust $ elemIndex c charset) - seed') $ length charset)

solve :: String -> String -> [String]
solve q hint = do
  decodedList <- map (\n -> ceasarDecode q n charset allowedStrRx) [1..(length charset)]
  filter (=~ hint) [decodedList]
  where
    charset :: String
    charset = "abcdefghijklmnopqrstuvwxyz .,-"
    allowedStrRx :: String
    allowedStrRx = "[abcdefghijklmnopqrstuvwxyz .,-]"
