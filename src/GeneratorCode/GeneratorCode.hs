{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module GeneratorCode.GeneratorCode(generateProgram) where

import Grammar.Grammar
import Data.List (intercalate)

generateProgram :: Program -> String
generateProgram (Program stmts) = unlines (map generateStatement stmts)

generateStatement :: Statement -> String
generateStatement (Assignment _ (Identifier ident) expr) =
    ident ++ " = " ++ generateExpression expr

generateStatement (PrintStatement expr) =
    "print(" ++ generateExpression expr ++ ")"

generateStatement (IfStatement cond trueBranch falseBranch) =
    lineBreak ++ "if " ++ generateExpression cond ++ ":" ++ lineBreak ++
    indent (concatMap generateStatement trueBranch) ++
    case falseBranch of
        Just false -> "else:" ++ lineBreak ++ indent (concatMap generateStatement false)
        Nothing -> ""

generateStatement (WhileStatement cond body) =
    lineBreak ++ "while " ++ generateExpression cond ++ ":" ++ lineBreak ++
    indent (concatMap generateStatement body)

generateStatement (PatternMatch (Identifier ident) cases) =
    lineBreak ++ "match " ++ ident ++ ":" ++ lineBreak ++
    concatMap generatePatternCase cases

generateStatement (FunctionDecl (Identifier name) params body ret) =
    lineBreak ++ "def " ++ name ++ "(" ++ paramList ++ "):" ++ lineBreak ++
    indent (concatMap generateStatement body) ++ 
    case ret of
        Just expr -> "    return " ++ generateExpression expr
        Nothing -> ""
  where
    paramList = intercalate ", " (map (\(Identifier param) -> param) params)

generateStatement (FunctionCall (Identifier name) args) =
    name ++ "(" ++ intercalate ", " (map generateExpression args) ++ ")"

generateStatement _ = error "Unsupported statement"

generateExpression :: Expression -> String
generateExpression (Term term) = generateTerm term
generateExpression (BinaryOp op left right) =
    generateExpression left ++ " " ++ generateOperator op ++ " " ++ generateExpression right
generateExpression (FunctCall (Identifier name) args) =
    name ++ "(" ++ intercalate ", " (map generateExpression args) ++ ")"
generateExpression _ = error "Unhandled expression"


generateTerm :: Term -> String
generateTerm (Number n) = show n
generateTerm (Ident (Identifier ident)) = ident
generateTerm (Boolean True) = "True"
generateTerm (Boolean False) = "False"
generateTerm (Str s) = show s

generateOperator :: Operator -> String
generateOperator (Arithmetical Add) = "+"
generateOperator (Arithmetical Sub) = "-"
generateOperator (Arithmetical Mul) = "*"
generateOperator (Arithmetical Div) = "/"
generateOperator (Logical And) = "&&"
generateOperator (Logical Or) = "||"
generateOperator (Relational Equal) = "=="
generateOperator (Relational NotEqual) = "!="
generateOperator (Relational LessThan) = "<"
generateOperator (Relational GreaterThan) = ">"
generateOperator (Relational LessThanOrEqual) = "<="
generateOperator (Relational GreaterThanOrEqual) = ">="

generatePatternCase :: PatternCase -> String
generatePatternCase (PatternCase pat stmts) =
    "    case " ++ generatePattern pat ++ ":" ++ lineBreak ++ "    " ++
    indent (concatMap generateStatement stmts)

generatePatternCase (OtherwiseCase stmts) =
    "    case _:" ++ lineBreak ++ "    " ++
    indent (concatMap generateStatement stmts)

generatePattern :: Pattern -> String
generatePattern (PatNumber n) = show n
generatePattern (PatString s) = show s

indent :: String -> String
indent = unlines . map ("    " ++) . lines

lineBreak :: String
lineBreak = "\n"
