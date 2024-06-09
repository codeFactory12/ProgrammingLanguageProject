module Tokenizer.Scanner (scanner) where

import Data.Char (isAlpha, isAlphaNum, isDigit, isSpace)

type Col = Int
type Line = Int
type Value = String
type Input = String

data Token = Token Type Value Line Col

data Type = StringToken
          | Keyword
          | Identifier
          | Number
          | Operator
          | Delimiter
          | TypeToken
          | Boolean
          | Comment
    deriving (Eq,Ord)

instance Show Token where
    show (Token t v l c) = show t ++ show v ++ " " ++ show l ++ " " ++ show c ++ "\n"

instance Show Type where
    show StringToken = "String: "
    show Keyword = "Keyword: "
    show Identifier = "Identifier: "
    show Number = "Number: "
    show Operator = "Operator: "
    show Delimiter = "Delimiter: "
    show TypeToken = "Type: "
    show Boolean = "Boolean: "
    show Comment = "Comment: "

-- Keywords and types
keywords :: [String]
keywords = ["let", "function", "return", "if", "else", "while", "match", "case", "print", "load", "as", "select", "filter", "group_by", "save", "apply", "@init", "@end"]

types :: [String]
types = ["int", "string", "bool"]

booleans :: [String]
booleans = ["true", "false"]

operator :: [String]
operator = ["+", "-", "*", "/", ">", "<", "==", "!=", ">=", "<=", "&&", "||"]

scanner :: Input -> [Token]
scanner xs = scan xs 1 1

scan :: Input -> Line -> Col -> [Token]
scan [] _ _ = []
scan (x:xs) l c
    | isSpace x = if x == '\n' then scan xs (l+1) 1 else scan xs l (c+1)
    | isAlpha x || x == '@' = lexIdentifier (x:xs) l c
    | isDigit x = lexNumber (x:xs) l c
    | x == '"' = lexString xs l (c+1) ""
    | isDKey x xs "//" || isDKey x xs "/*" = Token Keyword [x, head xs] l c : lexComment xs l (c+1)
    | x `elem` "{}()[],;=.+-*/><!&|" = lexDelimiterOrOperator (x:xs) l c
    | otherwise = scan xs l (c+1)

isDKey :: Char -> String -> String -> Bool
isDKey  _ [] [] = False
isDKey x xs key = [x,head xs] == key

lexIdentifier :: Input -> Line -> Col -> [Token]
lexIdentifier xs l c =
    let (name, rest) = span (\x -> isAlphaNum x || x == '@'|| x == '_') xs
        tokenType
          | name `elem` keywords = Keyword
          | name `elem` types = TypeToken
          | name `elem` booleans = Boolean
          | otherwise = Identifier
    in Token tokenType name l c : scan rest l (c + length name)

lexNumber :: Input -> Line -> Col -> [Token]
lexNumber xs l c =
    let (num, rest) = span isDigit xs
    in Token Number num l c : scan rest l (c + length num)

lexString :: Input -> Line -> Col -> String -> [Token]
lexString [] _ _ _ = error "Unterminated string"
lexString (x:xs) l c acc
    | x == '"' = Token StringToken acc l c : scan xs l (c + length acc + 2)
    | otherwise = lexString xs l (c+1) (acc ++ [x])

lexComment :: Input -> Line -> Col -> [Token]
lexComment ('/':xs) l c =
    let (com, rest) = break (== '\n') xs
    in Token Comment com l c : scan rest l (c + length com + 2)
lexComment ('*':xs) l c = lexMultiLineComment xs l (c+2) ""
lexComment xs l c = scan xs l c

lexMultiLineComment :: Input -> Line -> Col -> String -> [Token]
lexMultiLineComment [] _ _ _ = error "Unterminated multiline comment"
lexMultiLineComment ('*':'/':xs) l c acc = Token Comment acc l c : Token Keyword "*/" l c: scan xs l (c + length acc + 4)
lexMultiLineComment (x:xs) l c acc
    | x == '\n' = lexMultiLineComment xs (l+1) 1 (acc ++ [x])
    | otherwise = lexMultiLineComment xs l (c+1) (acc ++ [x])

lexDelimiterOrOperator :: Input -> Line -> Col -> [Token]
lexDelimiterOrOperator [] _ _ = []
lexDelimiterOrOperator (x:xs) l c
    | x `elem` "=><!&|;.+-(){}/*," =
        let (op, rest) = span (`elem` "=><!&|;.+-{}()/*,") (x:xs)
            tokenType = if op `elem` operator then Operator else Delimiter
        in Token tokenType op l c : scan rest l (c + length op)
    | otherwise = scan xs l (c+1)