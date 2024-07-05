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

        AST.SelectColumns ident _ -> validateDataType ident symTable

        AST.FilterRows ident _ -> validateDataType ident symTable

        AST.GroupBy ident _ _->  validateDataType ident symTable

        AST.SaveData ident _ -> validateDataType ident symTable

        AST.ApplyFunctions ident funcCall ->
            let symTable' = validateDataType ident symTable
            in analyzeStatement (uncurry AST.FunctionCall funcCall) symTable'

        AST.Comment _ -> symTable

        AST.PatternMatch ident cases ->
            let name = ST.nameFromIdentifier ident
                symTable' = case ST.lookupSymbol name symTable of
                    Just _  -> symTable
                    Nothing -> error $ "Pattern match on undefined variable: " ++ name
            in foldl (flip analyzePatternCase) symTable' cases

analyzeExpression :: AST.Expression -> ST.SymbolTable -> ST.SymbolTable
analyzeExpression expr symTable =
    case expr of
        AST.Term term -> analyzeTerm term symTable
        AST.BinaryOp _ left right ->
            let symTable' = analyzeExpression left symTable
            in analyzeExpression right symTable'
        AST.Filter ident _ -> validateDataType ident symTable
        AST.Group ident _ _ -> validateDataType ident symTable
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

validateDataType :: AST.Identifier -> ST.SymbolTable -> ST.SymbolTable
validateDataType ident symTable =
    case ST.lookupSymbol (ST.nameFromIdentifier ident) symTable of
        Just symbol ->
            if ST.getSymbolType symbol /= ST.DATA
            then error $ "Variable not of type DATA: " ++ ST.nameFromIdentifier ident
            else symTable
        Nothing -> error $ "Variable not in scope: " ++ ST.nameFromIdentifier ident

analyzePatternCase :: AST.PatternCase -> ST.SymbolTable -> ST.SymbolTable
analyzePatternCase patternCase symTable =
    case patternCase of
        AST.PatternCase pattern body ->
            analyzePattern pattern symTable body
        AST.OtherwiseCase body ->
            analyzeStatements body symTable

analyzePattern :: AST.Pattern -> ST.SymbolTable -> [AST.Statement] -> ST.SymbolTable
analyzePattern pattern symTable body =
    case pattern of
        AST.PatNumber num ->
            let symTable' = case ST.lookupSymbol (show num) symTable of
                    Just _ -> symTable
                    Nothing -> error $ "Pattern match on undefined number: " ++ show num
            in analyzeStatements body symTable'
        AST.PatString str ->
            let symTable' = case ST.lookupSymbol str symTable of
                    Just _ -> symTable
                    Nothing -> error $ "Pattern match on undefined string: " ++ str
            in analyzeStatements body symTable'
