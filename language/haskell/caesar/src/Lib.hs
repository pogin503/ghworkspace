module Lib
    (solve
    , caesarDecode
    , caesarEncode
    ) where

import Data.List (elemIndex)
import Data.Maybe (fromJust)
import Text.Regex.PCRE

caesarEncode :: String -> Int -> String -> String -> String
caesarEncode cipher seed charset allowedStrRx =
  map (\c -> if [c] =~ allowedStrRx
             then encode c seed
             else c) cipher
  where
    encode :: Char -> Int -> Char
    encode c seed' =
      charset !! (mod ((fromJust $ elemIndex c charset) + seed') $ length charset)

caesarDecode :: String -> Int -> String -> String -> String
caesarDecode plain seed charset allowedStrRx =
  map (\c -> if [c] =~ allowedStrRx
             then decode c seed
             else c) plain
  where
    decode :: Char -> Int ->  Char
    decode c seed' =
      charset !! (mod ((fromJust $ elemIndex c charset) - seed') $ length charset)

solve :: String -> String -> [String]
solve q hint = do
  decodedList <- map (\n -> caesarDecode q n charset allowedStrRx) [1..(length charset)]
  filter (=~ hint) [decodedList]
  where
    charset :: String
    charset = "abcdefghijklmnopqrstuvwxyz .,-"
    allowedStrRx :: String
    allowedStrRx = "[abcdefghijklmnopqrstuvwxyz .,-]"
