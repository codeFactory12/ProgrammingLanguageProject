module SemanticAnalyzer.SemanticAnalyzer (
    analyzeProgram,
    analyzeStatement,
    analyzeExpression
) where

import qualified SymbolTable.SymbolTable as ST
import qualified Grammar.Grammar as AST

analyzeProgram :: AST.Program -> ST.SymbolTable -> ST.SymbolTable
analyzeProgram (AST.Program stmts) symTable = analyzeStatements stmts symTable

analyzeStatements :: [AST.Statement] -> ST.SymbolTable -> ST.SymbolTable
analyzeStatements stmts symTable = foldl (flip analyzeStatement) symTable stmts

analyzeStatement :: AST.Statement -> ST.SymbolTable -> ST.SymbolTable
analyzeStatement stmt symTable =
    case stmt of
        AST.Assignment typ ident expr ->
            let name = ST.nameFromIdentifier ident
                sym = ST.Symbol name (ST.builtInTypeFromType typ) ST.LOCAL True
            in if ST.lookupSymbol name symTable == Nothing
                then ST.insertSymbol name sym symTable
                else symTable

        AST.FunctionDecl ident params body retExpr ->
            let name = ST.nameFromIdentifier ident
                sym = ST.Symbol name ST.STRING ST.GLOBAL False
                symTable' = ST.insertSymbol name sym symTable
                paramSyms = map (\p -> ST.Symbol (ST.nameFromIdentifier p) ST.STRING ST.LOCAL False) params
                symTable'' = foldl (\tbl param -> ST.insertSymbol (ST.symbolName param) param tbl) symTable' paramSyms
            in analyzeStatements body symTable''

        AST.FunctionCall ident args ->
            let name = ST.nameFromIdentifier ident
            in case ST.lookupSymbol name symTable of
                Just _  -> symTable
                Nothing -> error $ "Function not defined: " ++ name

        AST.IfStatement cond thenStmts elseStmts ->
            let symTable' = analyzeExpression cond symTable
                symTableThen = analyzeStatements thenStmts symTable'
                symTableElse = case elseStmts of
                    Just stmts -> analyzeStatements stmts symTable'
                    Nothing    -> symTable'
            in symTableElse


        AST.WhileStatement cond body ->
            let symTable' = analyzeExpression cond symTable
            in analyzeStatements body symTable'

        AST.PrintStatement expr -> analyzeExpression expr symTable

        AST.LoadData _ ident ->
            let name = ST.nameFromIdentifier ident
                sym = ST.Symbol name ST.DATA ST.GLOBAL True
            in ST.insertSymbol name sym symTable

        AST.SelectColumns ident _ ->
            case ST.lookupSymbol (ST.nameFromIdentifier ident) symTable of
            Just symbol ->
                if ST.getSymbolType symbol /= ST.DATA
                then error $ "Variable not of type DATA: " ++ ST.nameFromIdentifier ident
                else symTable
            Nothing -> error $ "Variable not in scope: " ++ ST.nameFromIdentifier ident

        AST.FilterRows ident _ -> 
            case ST.lookupSymbol (ST.nameFromIdentifier ident) symTable of
            Just symbol ->
                if ST.getSymbolType symbol /= ST.DATA
                then error $ "Variable not of type DATA: " ++ ST.nameFromIdentifier ident
                else symTable
            Nothing -> error $ "Variable not in scope: " ++ ST.nameFromIdentifier ident

        AST.GroupBy ident _ ->  
            case ST.lookupSymbol (ST.nameFromIdentifier ident) symTable of
            Just symbol ->
                if ST.getSymbolType symbol /= ST.DATA
                then error $ "Variable not of type DATA: " ++ ST.nameFromIdentifier ident
                else symTable
            Nothing -> error $ "Variable not in scope: " ++ ST.nameFromIdentifier ident

        AST.SaveData ident _ -> 
            case ST.lookupSymbol (ST.nameFromIdentifier ident) symTable of
            Just symbol ->
                if ST.getSymbolType symbol /= ST.DATA
                then error $ "Variable not of type DATA: " ++ ST.nameFromIdentifier ident
                else symTable
            Nothing -> error $ "Variable not in scope: " ++ ST.nameFromIdentifier ident

        AST.ApplyFunctions ident funcCall -> symTable

        AST.Comment _ -> symTable

        AST.PatternMatch ident cases -> symTable

analyzeExpression :: AST.Expression -> ST.SymbolTable -> ST.SymbolTable
analyzeExpression expr symTable =
    case expr of
        AST.Term term -> analyzeTerm term symTable
        AST.BinaryOp _ left right ->
            let symTable' = analyzeExpression left symTable
            in analyzeExpression right symTable'
        AST.Filter _ _ -> symTable
        AST.Group _ _ -> symTable
        AST.FunctCall _ _ -> symTable

analyzeTerm :: AST.Term -> ST.SymbolTable -> ST.SymbolTable
analyzeTerm term symTable =
    case term of
        AST.Number _ -> symTable
        AST.Ident ident ->
            let name = ST.nameFromIdentifier ident
            in case ST.lookupSymbol name symTable of
                Just _  -> symTable
                Nothing -> error $ "Variable not in scope: " ++ name
        AST.Boolean _ -> symTable
        AST.Str _ -> symTable
        AST.Expr expr -> analyzeExpression expr symTable
