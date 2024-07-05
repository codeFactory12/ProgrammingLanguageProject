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
            in case ST.lookupSymbol name symTable of
                Just symbol ->
                    let symTable' = analyzePatternCases symbol cases symTable
                    in symTable'
                Nothing ->
                    let sym = ST.Symbol name ST.STRING ST.LOCAL True
                        symTable' = ST.insertSymbol name sym symTable
                    in analyzePatternCases sym cases symTable'

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

analyzePatternCases :: ST.Symbol -> [AST.PatternCase] -> ST.SymbolTable -> ST.SymbolTable
analyzePatternCases symbol cases symTable = foldl (analyzePatternCase symbol) symTable cases

analyzePatternCase :: ST.Symbol -> ST.SymbolTable -> AST.PatternCase -> ST.SymbolTable
analyzePatternCase symbol symTable (AST.PatternCase pattern stmts) =
    if validatePattern symbol pattern
    then analyzeStatements stmts symTable
    else symTable
analyzePatternCase _ symTable (AST.OtherwiseCase stmts) = analyzeStatements stmts symTable

validatePattern :: ST.Symbol -> AST.Pattern -> Bool
validatePattern symbol pattern =
    case pattern of
        AST.PatNumber _ -> ST.getSymbolType symbol == ST.INTEGER
        AST.PatString _ -> ST.getSymbolType symbol == ST.STRING
