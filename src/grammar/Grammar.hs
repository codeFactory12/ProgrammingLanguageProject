module Grammar () where

data Program = Program [Statement] 
  deriving (Show)

data Statement = Assignment Type Identifier Expression
               | FunctionDecl Identifier [Identifier] [Statement] (Maybe Expression)
               | FunctionCall Identifier [Expression]
               | IfStatement Expression [Statement] (Maybe [Statement])
               | WhileStatement Expression [Statement]
               | PatternMatch Identifier [PatternCase]
               | PrintStatement Expression
               | LoadData String Identifier
               | SelectColumns Identifier [Identifier]
               | FilterRows Identifier Expression
               | GroupBy Identifier [Identifier]
               | SaveData Identifier String
               | ApplyFunctions Identifier FunctionCall
               | Comment String
               deriving (Show)

data Type = TypeInt | TypeString | TypeBool 
  deriving (Show)

data Expression = Term Term
                | BinaryOp Operator Expression Expression
                deriving (Show)

data Term = Number Integer
          | Ident Identifier
          | Boolean Bool
          | Str String
          | Expr Expression
          deriving (Show)

data Operator = Arithmetical Arithmetical | Logical Logical | Relational Relational
              deriving (Show)

data Arithmetical = Add | Sub | Mul | Div
  deriving (Show)

data Logical = And | Or 
  deriving (Show)

data Relational = Equal | NotEqual | LessThan | GreaterThan | LessThanOrEqual | GreaterThanOrEqual
  deriving (Show)

data Identifier = Identifier String 
  deriving (Show)

data PatternCase = PatternCase Pattern [Statement] 
  deriving (Show)

data Pattern = PatNumber Integer | PatString String 
  deriving (Show)

type FunctionCall = (Identifier, [Expression])
