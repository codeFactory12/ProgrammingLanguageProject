module Main (main) where

import Lib
import Tokenizer.Scanner

main :: IO ()
main = do input <- readFile "program.gozu"
          print input
          let token = scanner input
          putStrLn(show token)
