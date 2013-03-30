module AST where

import Data.Word

data PrimitiveTypes = TBool Bool
                    | TShort Word16
                    | TInt Word32
                    | TLong Word64
                    | TChar Word8
                    | TFloat Float
                    | TDouble Double
                    {-| TPtr PrimitiveTypes-}
                    | TVoid
                    | TArray [PrimitiveTypes] Integer
                    | TConst PrimitiveTypes
                    deriving (Show, Eq)

data BinExpr = Bool Bool
           | Not BinExpr
           | BoolBinExpr BoolBinOp BinExpr BinExpr
           | CompBinExpr CompBinOp ArithExpr ArithExpr
           deriving (Show)

data BoolBinOp = And
               | Or
               deriving (Show)

data CompBinOp = Equal
          | Lesser
          | Greater
          deriving (Show)

data ArithBinOp = Add
                | Sub
                | Mult
                | Div
                | Mod
                deriving (Show)

data ArithExpr = Var
               | Neg ArithExpr
               | ArithBin ArithBinOp ArithExpr ArithExpr
               deriving (Show)

data Expr = BExpr BinExpr
          deriving (Show)

data VarType = VarType String
          deriving (Show)

data Stmt = Seq [Stmt]
          | Decl VarType String
          | Assign String Expr
          | If Expr Stmt Stmt
          | While Expr Stmt
          | Skip
          deriving (Show)
