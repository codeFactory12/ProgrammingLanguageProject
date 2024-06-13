{-# LANGUAGE FlexibleContexts #-}

module Parser.Parser (parseProgram) where

import Text.Parsec
import Text.Parsec.String (Parser)
import Text.Parsec.Language (emptyDef)
import qualified Text.Parsec.Token as Token

import Grammar.Grammar

languageDef =
  emptyDef { Token.commentLine     = "//"
           , Token.commentStart    = "/*"
           , Token.commentEnd      = "*/"
           , Token.nestedComments  = True
           , Token.identStart      = letter
           , Token.identLetter     = alphaNum <|> oneOf "_'"
           , Token.opStart         = Token.opLetter emptyDef
           , Token.opLetter        = oneOf ":!#$%&*+./<=>?@\\^|-~"
           , Token.reservedNames   = ["let", "int", "bool", "string", "true", "false"]
           , Token.reservedOpNames = ["=", ";"]
           , Token.caseSensitive   = True
           }

lexer = Token.makeTokenParser languageDef

identifier = Identifier <$> Token.identifier lexer
reserved = Token.reserved lexer
reservedOp = Token.reservedOp lexer
semi = Token.semi lexer
parens = Token.parens lexer
integer = Token.integer lexer
stringLiteral = Token.stringLiteral lexer
whiteSpace = Token.whiteSpace lexer
comma = Token.comma lexer
reservedLet = reserved "let"

typeParser :: Parser Type
typeParser = (reserved "int" >> return TypeInt)
         <|> (reserved "bool" >> return TypeBool)
         <|> (reserved "string" >> return TypeString)

term :: Parser Term
term = (Number <$> integer)
    <|> (Boolean True <$ reserved "true")
    <|> (Boolean False <$ reserved "false")
    <|> (Str <$> stringLiteral)
    <|> (Ident <$> identifier)

expression :: Parser Expression
expression = Term <$> term

assignment :: Parser Statement
assignment = do
  reservedLet
  typ <- typeParser
  name <- identifier
  reservedOp "="
  Assignment typ name <$> expression

parseProgram :: Parser Program
parseProgram = whiteSpace *> (Program <$> many assignment) <* eof
