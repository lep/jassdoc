
module Jass.Types
    ( Expr
    , Stmt
    , LVar
    , VarDef
    , Toplevel
    , Programm
    , Block
    , Name
    , Type
    , Constant (..)
    , Stringtype
    ) where

import Data.ByteString.Lazy (ByteString)

data Expr
data Stmt
data LVar
data VarDef
data Toplevel
data Block
data Programm

data Constant = Const | Normal
    deriving (Eq, Show)

type Name = String
type Type = String
type Stringtype = String
