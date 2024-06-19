module Lib
    ( someFunc
    ) where

import Parser.Parser (parseProgram)
import Text.Parsec

someFunc :: IO ()
someFunc = do
    --let input = "let int result = 0;\nlet bool flag = true;\nlet string message = \"Process completed\";"
    input <- readFile "program.gozu"
    case parse parseProgram "" input of
        Left err -> print err
        Right prog -> print prog