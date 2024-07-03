module SymbolTable.SymbolTable
  ( insertSymbol,
    lookupSymbol,
    updateSymbol,
    deleteSymbol,
    symbolExists,
    populateSymbolTable,
    SymbolTable,
    Scope (..),
    Symbol (..),
    BuiltInType (..),
    builtInTypeFromType,
    nameFromIdentifier,
  )
where

import qualified Data.Map as Map
import qualified Grammar.Grammar as AST

type SymbolTable = Map.Map String Symbol

data Scope = GLOBAL | LOCAL | BLOCK deriving (Show, Eq)

data BuiltInType = INTEGER | STRING | BOOL | DATA deriving (Show, Eq)

data Symbol = Symbol
  { symbolName :: String,
    symbolType :: BuiltInType,
    symbolScope :: Scope,
    isMutable :: Bool
  }
  deriving (Show, Eq)

insertSymbol :: String -> Symbol -> SymbolTable -> SymbolTable
insertSymbol name symbol symbolTable = if symbolExists name symbolTable
                     then symbolTable
                     else Map.insert name symbol symbolTable

lookupSymbol :: String -> SymbolTable -> Maybe Symbol
lookupSymbol name table = Map.lookup name table

deleteSymbol :: String -> SymbolTable -> SymbolTable
deleteSymbol = Map.delete

updateSymbol :: String -> Symbol -> SymbolTable -> SymbolTable
updateSymbol name symbol symbolTable = Map.insert name symbol symbolTable

symbolExists :: String -> SymbolTable -> Bool
symbolExists name symbolTable = Map.member name symbolTable

builtInTypeFromType :: AST.Type -> BuiltInType
builtInTypeFromType AST.TypeInt = INTEGER
builtInTypeFromType AST.TypeString = STRING
builtInTypeFromType AST.TypeBool = BOOL
builtInTypeFromType AST.TypeData = DATA

nameFromIdentifier :: AST.Identifier -> String
nameFromIdentifier (AST.Identifier s) = s

populateSymbolTable :: [AST.Statement] -> SymbolTable -> SymbolTable
populateSymbolTable stmts table = foldl processStatement table stmts
  where
    processStatement :: SymbolTable -> AST.Statement -> SymbolTable
    processStatement tbl (AST.Assignment typ ident _) =
      let name = nameFromIdentifier ident
          sym = Symbol name (builtInTypeFromType typ) LOCAL True
       in insertSymbol name sym tbl
    processStatement tbl (AST.FunctionDecl ident params _ _) =
      let name = nameFromIdentifier ident
          sym = Symbol name STRING GLOBAL False
          tbl' = insertSymbol name sym tbl
       in foldl
            ( \t p ->
                let pname = nameFromIdentifier p
                    psym = Symbol pname STRING LOCAL False
                 in insertSymbol pname psym t
            )
            tbl'
            params
    processStatement tbl _ = tbl
