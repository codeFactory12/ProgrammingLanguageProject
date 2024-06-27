module Lib
    ( someFunc
    ) where

import Parser.Parser (parseProgram)
import Text.Parsec

someFunc :: IO ()
someFunc = do
    input <- readFile "program.gozu"
    case parse parseProgram "" input of
        Left err -> print err
        Right prog -> print prog