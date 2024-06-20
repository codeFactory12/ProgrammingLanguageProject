{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use $>" #-}
module Parser.Parser (parseProgram) where

import Data.Functor.Identity
import Grammar.Grammar
import Text.Parsec
import qualified Text.Parsec.Expr as Ex
import Text.Parsec.Language (emptyDef)
import Text.Parsec.String (Parser)
import qualified Text.Parsec.Token as Token

languageDef :: Token.GenLanguageDef String u Identity
languageDef =
  emptyDef
    { Token.commentLine = "//",
      Token.commentStart = "/*",
      Token.commentEnd = "*/",
      Token.nestedComments = True,
      Token.identStart = letter,
      Token.identLetter = alphaNum <|> oneOf "_'",
      Token.opStart = Token.opLetter emptyDef,
      Token.opLetter = oneOf ":!#$%&*+./<=>?@\\^|-~",
      Token.reservedNames =
        [ "@init",
          "let",
          "int",
          "bool",
          "string",
          "true",
          "false",
          "while",
          "if",
          "else",
          "match",
          "case",
          "otherwise",
          "print",
          "function",
          "return",
          "load",
          "as",
          "select",
          "filter",
          "group_by",
          "save",
          "@end"
        ],
      Token.reservedOpNames = ["=", ";", "=>"],
      Token.caseSensitive = True
    }

lexer :: Token.GenTokenParser String u Identity
lexer = Token.makeTokenParser languageDef

identifier :: ParsecT String u Identity Identifier
identifier = Identifier <$> Token.identifier lexer

reserved :: String -> ParsecT String u Identity ()
reserved = Token.reserved lexer

reservedOp :: String -> ParsecT String u Identity ()
reservedOp = Token.reservedOp lexer

semiColon :: ParsecT String u Identity String
semiColon = Token.semi lexer

parens :: ParsecT String u Identity a -> ParsecT String u Identity a
parens = Token.parens lexer

integer :: ParsecT String u Identity Integer
integer = Token.integer lexer

stringLiteral :: ParsecT String u Identity String
stringLiteral = Token.stringLiteral lexer

whiteSpace :: ParsecT String u Identity ()
whiteSpace = Token.whiteSpace lexer

comma :: ParsecT String u Identity String
comma = Token.comma lexer

braces :: ParsecT String u Identity a -> ParsecT String u Identity a
braces = Token.braces lexer

typeParser :: Parser Type
typeParser =
  (reserved "int" >> return TypeInt)
    <|> (reserved "bool" >> return TypeBool)
    <|> (reserved "string" >> return TypeString)

operatorTable :: Ex.OperatorTable String () Identity Expression
operatorTable =
  [ [ Ex.Infix (reservedOp "*" *> return (BinaryOp (Arithmetical Mul))) Ex.AssocLeft,
      Ex.Infix (reservedOp "/" *> return (BinaryOp (Arithmetical Div))) Ex.AssocLeft
    ],
    [ Ex.Infix (reservedOp "+" *> return (BinaryOp (Arithmetical Add))) Ex.AssocLeft,
      Ex.Infix (reservedOp "-" *> return (BinaryOp (Arithmetical Sub))) Ex.AssocLeft
    ],
    [ Ex.Infix (reservedOp "==" *> return (BinaryOp (Relational Equal))) Ex.AssocNone,
      Ex.Infix (reservedOp "!=" *> return (BinaryOp (Relational NotEqual))) Ex.AssocNone,
      Ex.Infix (reservedOp "<" *> return (BinaryOp (Relational LessThan))) Ex.AssocNone,
      Ex.Infix (reservedOp ">" *> return (BinaryOp (Relational GreaterThan))) Ex.AssocNone,
      Ex.Infix (reservedOp "<=" *> return (BinaryOp (Relational LessThanOrEqual))) Ex.AssocNone,
      Ex.Infix (reservedOp ">=" *> return (BinaryOp (Relational GreaterThanOrEqual))) Ex.AssocNone
    ],
    [ Ex.Infix (reservedOp "&&" *> return (BinaryOp (Logical And))) Ex.AssocRight,
      Ex.Infix (reservedOp "||" *> return (BinaryOp (Logical Or))) Ex.AssocRight
    ]
  ]

term :: Parser Expression
term =
  try functionCallExpr
    <|> parens expression
    <|> ( Term
            <$> ( Number <$> integer
                    <|> Boolean True <$ reserved "true"
                    <|> Boolean False <$ reserved "false"
                    <|> Expr <$> parens expression
                    <|> Str <$> stringLiteral
                    <|> Ident <$> identifier
                )
        )

expression :: Parser Expression
expression = Ex.buildExpressionParser operatorTable term

assignment :: Parser Statement
assignment = try letAssignment <|> simpleAssignment

letAssignment :: Parser Statement
letAssignment =
  Assignment
    <$> (reserved "let" *> typeParser)
    <*> identifier
    <*> (reservedOp "=" *> expression)
    <* semiColon

simpleAssignment :: Parser Statement
simpleAssignment = do
  Assignment TypeInt
    <$> identifier
    <*> (reservedOp "=" *> (try filterRowsExpr <|> try groupByExpr <|> expression))
    <* semiColon

whileStatement :: Parser Statement
whileStatement =
  WhileStatement
    <$> (reserved "while" *> parens expression)
    <*> braces (many statement)

ifStatement :: Parser Statement
ifStatement =
  IfStatement
    <$> (reserved "if" *> parens expression)
    <*> braces (many statement)
    <*> optionMaybe (reserved "else" *> braces (many statement))

printStatement :: Parser Statement
printStatement =
  PrintStatement
    <$> (reserved "print" *> parens expression <* semiColon)

patternCase :: Parser PatternCase
patternCase =
  PatternCase
    <$> (reserved "case" *> patternParser <* reservedOp "=>")
    <*> braces (many statement)

otherwiseCase :: Parser PatternCase
otherwiseCase = do
  reserved "otherwise"
  reservedOp "=>"
  stmts <- braces (many statement)
  return $ OtherwiseCase stmts

patternParser :: Parser Pattern
patternParser =
  (PatNumber <$> integer)
    <|> (PatString <$> stringLiteral)

patternMatch :: Parser Statement
patternMatch = do
  resultIdentifier <- reserved "match" *> identifier
  cases <- braces (many (try patternCase <|> otherwiseCase))
  return $ PatternMatch resultIdentifier cases

stringCsv :: Parser String
stringCsv = do
  _ <- char '"'
  filename <- manyTill anyChar (try (string ".csv"))
  _ <- char '"'
  skipMany space
  return (filename ++ ".csv")

loadData :: Parser Statement
loadData = do
  reserved "load" *> skipMany space
  filename <- stringCsv
  dataIdentifier <- reserved "as" *> identifier <* semiColon
  return $ LoadData filename dataIdentifier

selectColumns :: Parser Statement
selectColumns = do
  identifierData <- identifier
  char '.' *> reserved "select" *> skipMany space
  columns <- parens (identifier `sepBy` comma) <* semiColon
  return $ SelectColumns identifierData columns

filerRows :: Parser Statement
filerRows = do
  identifierData <- identifier
  char '.' *> reserved "filter" *> skipMany space
  expressionData <- parens expression <* semiColon
  return $ FilterRows identifierData expressionData

filterRowsExpr :: Parser Expression
filterRowsExpr = do
  identifierData <- identifier
  char '.' *> reserved "filter" *> skipMany space
  expressionData <- parens expression
  return $ Filter identifierData expressionData

groupBy :: Parser Statement
groupBy = do
  identifierData <- identifier
  char '.' *> reserved "group_by" *> skipMany space
  columns <- parens (identifier `sepBy` comma) <* semiColon
  return $ GroupBy identifierData columns

groupByExpr :: Parser Expression
groupByExpr = do
  identifierData <- identifier
  char '.' *> reserved "group_by" *> skipMany space
  columns <- parens (identifier `sepBy` comma)
  return $ Group identifierData columns

saveData :: Parser Statement
saveData = do
  identifierData <- identifier
  char '.' *> reserved "save" *> skipMany space
  filename <- parens stringCsv <* semiColon
  return $ SaveData identifierData filename

returnExpr :: Parser Expression
returnExpr = reserved "return" *> expression <* semiColon

functionDeclaration :: Parser Statement
functionDeclaration = do
  reserved "function"
  name <- identifier
  args <- parens (identifier `sepBy` comma)
  _ <- char '{' <* whiteSpace
  body <- do
    bodyStatement <- try (many statement)
    returnExpression <- optionMaybe returnExpr
    return (bodyStatement, returnExpression)
  let (bodyStatements, hasReturn) = body
  _ <- char '}' <* whiteSpace
  return $ FunctionDecl name args bodyStatements hasReturn

functionCall :: Parser Statement
functionCall = do
  name <- identifier
  args <- parens (expression `sepBy` comma) <* semiColon
  return $ FunctionCall name args

functionCallExpr :: Parser Expression
functionCallExpr = do
  name <- identifier
  args <- parens (expression `sepBy` comma)
  return $ FunctCall name args

statement :: Parser Statement
statement =
  try assignment
    <|> try whileStatement
    <|> try ifStatement
    <|> try printStatement
    <|> try patternMatch
    <|> try loadData
    <|> try selectColumns
    <|> try filerRows
    <|> try groupBy
    <|> try saveData
    <|> try functionDeclaration
    <|> try functionCall

initParser :: Parser Program
initParser = do
  reserved "@init"
  p <- many statement
  reserved "@end"
  eof
  return $ Program p

parseProgram :: Parser Program
parseProgram = whiteSpace *> initParser <* eof
