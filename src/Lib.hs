module Lib
    ( someFunc
    ) where

import Parser.Parser (parseProgram)
import Text.Parsec
import qualified SymbolTable.SymbolTable as ST
import qualified SemanticAnalyzer.SemanticAnalyzer as SA
import qualified Data.Map as Map
import GeneratorCode.GeneratorCode (generateProgram)

someFunc :: IO ()
someFunc = do
    --let input = "let int result = 0;\nlet bool flag = true;\nlet string message = \"Process completed\";"
    -- testCasesSuccess.gozu
    -- testCasesFailed.gozu
    -- program.gozu
    -- testGeneratorCode.gozu
    input <- readFile "testGeneratorCode.gozu"
    case parse parseProgram "" input of
        Left err -> print err
        Right prog -> do
            let initialSymbolTable = ST.populateSymbolTable [] Map.empty
            let finalSymbolTable = SA.analyzeProgram prog initialSymbolTable
            putStrLn "Symbol Table after Semantic Analysis:"
            mapM_ print (Map.toList finalSymbolTable)
            let pythonCode = generateProgram prog
            writeFile "program.py" pythonCode
            putStrLn "Generation successful, output written to program.py"
        