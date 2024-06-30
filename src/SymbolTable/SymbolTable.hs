module SymbolTable.SymbolTable
    ( insertSymbol,
        lookupSymbol,
        deleteSymbol,
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

data BuiltInType = INTEGER | STRING | BOOL deriving (Show, Eq)

data Symbol = Symbol String BuiltInType deriving (Show, Eq)

insertSymbol :: String -> Symbol -> SymbolTable -> SymbolTable
insertSymbol name symbol symbolTable = Map.insert name symbol symbolTable

lookupSymbol :: String -> SymbolTable -> Maybe Symbol
lookupSymbol name table = Map.lookup name table

deleteSymbol :: String -> SymbolTable -> SymbolTable
deleteSymbol = Map.delete

builtInTypeFromType :: AST.Type -> BuiltInType
builtInTypeFromType AST.TypeInt = INTEGER
builtInTypeFromType AST.TypeString = STRING
builtInTypeFromType AST.TypeBool = BOOL

nameFromIdentifier :: AST.Identifier -> String
nameFromIdentifier (AST.Identifier s) = s
