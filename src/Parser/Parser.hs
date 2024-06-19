{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use $>" #-}

module Parser.Parser (parseProgram) where

import Text.Parsec
import Text.Parsec.String (Parser)
import Text.Parsec.Language (emptyDef)
import qualified Text.Parsec.Token as Token
import qualified Text.Parsec.Expr as Ex

import Grammar.Grammar
import Data.Functor.Identity

languageDef =
  emptyDef { Token.commentLine     = "//"
           , Token.commentStart    = "/*"
           , Token.commentEnd      = "*/"
           , Token.nestedComments  = True
           , Token.identStart      = letter
           , Token.identLetter     = alphaNum <|> oneOf "_'"
           , Token.opStart         = Token.opLetter emptyDef
           , Token.opLetter        = oneOf ":!#$%&*+./<=>?@\\^|-~"
           , Token.reservedNames   = ["let", "int", "bool", "string", "true", "false", "while", "if", "else", "match","case","otherwise","print"]
           , Token.reservedOpNames = ["=", ";", "=>"]
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
braces = Token.braces lexer

typeParser :: Parser Type
typeParser = (reserved "int" >> return TypeInt)
         <|> (reserved "bool" >> return TypeBool)
         <|> (reserved "string" >> return TypeString)

operatorTable :: Ex.OperatorTable String () Identity Expression
operatorTable =
    [ [ Ex.Infix  (reservedOp "*" *> return (BinaryOp (Arithmetical Mul))) Ex.AssocLeft
      , Ex.Infix  (reservedOp "/" *> return (BinaryOp (Arithmetical Div))) Ex.AssocLeft
      ]
    , [ Ex.Infix  (reservedOp "+" *> return (BinaryOp (Arithmetical Add))) Ex.AssocLeft
      , Ex.Infix  (reservedOp "-" *> return (BinaryOp (Arithmetical Sub))) Ex.AssocLeft
      ]
    , [ Ex.Infix  (reservedOp "==" *> return (BinaryOp (Relational Equal))) Ex.AssocNone
      , Ex.Infix  (reservedOp "!=" *> return (BinaryOp (Relational NotEqual))) Ex.AssocNone
      , Ex.Infix  (reservedOp "<"  *> return (BinaryOp (Relational LessThan))) Ex.AssocNone
      , Ex.Infix  (reservedOp ">"  *> return (BinaryOp (Relational GreaterThan))) Ex.AssocNone
      , Ex.Infix  (reservedOp "<=" *> return (BinaryOp (Relational LessThanOrEqual))) Ex.AssocNone
      , Ex.Infix  (reservedOp ">=" *> return (BinaryOp (Relational GreaterThanOrEqual))) Ex.AssocNone
      ]
    , [ Ex.Infix  (reservedOp "&&" *> return (BinaryOp (Logical And))) Ex.AssocRight
      , Ex.Infix  (reservedOp "||" *> return (BinaryOp (Logical Or))) Ex.AssocRight
      ]
    ]

term :: Parser Expression
term =  parens expression
    <|> (Term <$> (Number <$> integer
              <|> Boolean True <$ reserved "true"
              <|> Boolean False <$ reserved "false"
              <|> Str <$> stringLiteral
              <|> Ident <$> identifier))

expression :: Parser Expression
expression = Ex.buildExpressionParser operatorTable term

assignment :: Parser Statement
assignment = try letAssignment <|> simpleAssignment

letAssignment :: Parser Statement
letAssignment = Assignment 
             <$> (reservedLet *> typeParser) 
             <*> identifier 
             <*> (reservedOp "=" *> expression) 
             <* semi

simpleAssignment :: Parser Statement
simpleAssignment =
    Assignment TypeInt
    <$> identifier
    <*> (reservedOp "=" *> expression)
    <* semi

whileStatement :: Parser Statement
whileStatement = WhileStatement
             <$> (reserved "while" *> parens expression)
             <*> braces (many statement)

ifStatement :: Parser Statement
ifStatement = IfStatement
          <$> (reserved "if" *> parens expression)
          <*> braces (many statement)
          <*> optionMaybe (reserved "else" *> braces (many statement))

printStatement :: Parser Statement
printStatement = PrintStatement
             <$> (reserved "print" *> parens expression <* semi)

patternCase :: Parser PatternCase
patternCase = PatternCase
    <$> (reserved "case" *> patternParser <* reservedOp "=>")
    <*> braces (many statement)

otherwiseCase :: Parser PatternCase
otherwiseCase = do
    reserved "otherwise"
    reservedOp "=>"
    stmts <- braces (many statement)
    return $ OtherwiseCase stmts

patternParser :: Parser Pattern
patternParser = (PatNumber <$> integer)
      <|> (PatString <$> stringLiteral)

patternMatch :: Parser Statement
patternMatch = do
    identifier <- reserved "match" *> identifier
    cases <- braces (many (try patternCase <|> otherwiseCase))
    return $ PatternMatch identifier cases

statement :: Parser Statement
statement = try assignment
        <|> try whileStatement
        <|> try ifStatement
        <|> try printStatement
        <|> try patternMatch

parseProgram :: Parser Program
parseProgram = whiteSpace *> (Program <$> many statement) <* eof
