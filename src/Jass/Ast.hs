{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE OverloadedStrings #-}


module Jass.Ast
    ( Ast (..)
    , Expr
    , Stmt
    , LVar
    , VarDef
    , Toplevel
    , Name
    , Type
    , Constant (..)
    ) where

import Data.Composeable
import Data.Traversable
import Control.Applicative
import Control.Arrow

import Data.Aeson as Aeson hiding (Bool, String, Null)


import Jass.Types as Jass
import qualified Data.Aeson as Aeson hiding (Bool)

-- plain AST
data Ast ann a where
    Programm :: [Ast ann Toplevel] -> Ast ann Programm

    Native :: ann -> Constant -> Name -> [(Type, Name)] -> Type -> Ast ann Toplevel
    Function :: ann -> Constant -> Name -> [(Type, Name)] -> Type -> [Ast ann Stmt] -> Ast ann Toplevel
    Global :: Ast ann VarDef -> Ast ann Toplevel
    Typedef :: ann -> Type -> Type -> Ast ann Toplevel


    Set :: Ast ann LVar -> Ast ann Expr -> Ast ann Stmt
    Local :: Ast ann VarDef -> Ast ann Stmt
    If :: Ast ann Expr -> [Ast ann Stmt] -> [(Ast ann Expr, [Ast ann Stmt])] -> Maybe [Ast ann Stmt] -> Ast ann Stmt
    Loop :: [Ast ann Stmt] -> Ast ann Stmt
    Exitwhen :: Ast ann Expr -> Ast ann Stmt
    Return :: Maybe (Ast ann Expr) -> Ast ann Stmt

    Call :: Name -> [Ast ann Expr] -> Ast ann a

    Var :: Ast ann LVar -> Ast ann Expr
    Int :: Stringtype -> Ast ann Expr
    Rawcode :: Stringtype -> Ast ann Expr
    Real :: Stringtype -> Ast ann Expr
    Bool :: Bool -> Ast ann Expr
    String :: Stringtype -> Ast ann Expr
    Code :: Name -> Ast ann Expr
    Null :: Ast ann Expr

    AVar :: Name -> Ast ann Expr -> Ast ann LVar
    SVar :: Name -> Ast ann LVar

    ADef :: ann -> Name -> Type -> Ast ann VarDef
    SDef :: ann -> Constant -> Name -> Type -> Maybe (Ast ann Expr) -> Ast ann VarDef

deriving instance Show ann => Show (Ast ann a)
deriving instance Eq ann => Eq (Ast ann a)


instance ToJSON Constant where
  toJSON Jass.Const = "constant" 
  toJSON Jass.Normal = ""

instance ToJSON ann => ToJSON (Ast ann x) where
  toJSON x =
    case x of
      SDef ann constantness name ty init ->
        object [
          "type" .= Aeson.String "vardef",
          "kind" .= Aeson.String "sdef",
          "annotations" .= ann,
          "constant" .= constantness,
          "name" .= name,
          "type" .= ty,
          "expr" .= init
        ]

      ADef ann name ty ->
        object [
          "type" .= Aeson.String "vardef",
          "kind" .= Aeson.String "adef",
          "annotations" .= ann,
          "type" .= ty,
          "name" .= name
        ]

      AVar name idx ->
        object [
          "type" .= Aeson.String "expr",
          "kind" .= Aeson.String "avar",
          "name" .= name,
          "index" .= idx
        ]

      SVar name ->
        object [
          "type" .= Aeson.String "expr",
          "kind" .= Aeson.String "svar",
          "name" .= name
        ]

      Var lvar -> toJSON lvar
      Null ->
        object [
          "type" .= Aeson.String "expr",
          "kind" .= Aeson.String "null"
        ]
      Code fn ->
        object [
          "type" .= Aeson.String "expr",
          "kind" .= Aeson.String "code",
          "name" .= fn
        ]

      String s ->
        object [
          "type" .= Aeson.String "expr",
          "kind" .= Aeson.String "string",
          "value" .= s
        ]

      Bool b ->
        object [
          "type" .= Aeson.String "expr",
          "kind" .= Aeson.String "bool",
          "value" .= b
        ]
      Real v ->
        object [
          "type" .= Aeson.String "expr",
          "kind" .= Aeson.String "real",
          "value" .= v
        ]
      Rawcode v ->
        object [
          "type" .= Aeson.String "expr",
          "kind" .= Aeson.String "rawcode",
          "value" .= v
        ]
      Int v ->
        object [
          "type" .= Aeson.String "expr",
          "kind" .= Aeson.String "integer",
          "value" .= v
        ]

      Call name args ->
        object [
          "type" .= Aeson.String "call",
          "kind" .= Aeson.String "call",
          "name" .= name,
          "args" .= args
        ]

      Return v ->
        object [
          "type" .= Aeson.String "stmt",
          "kind" .= Aeson.String "return",
          "value" .= v
        ]
      Exitwhen v ->
        object [
          "type" .= Aeson.String "stmt",
          "kind" .= Aeson.String "exitwhen",
          "value" .= v
        ]
      Loop body ->
        object [
          "type" .= Aeson.String "stmt",
          "kind" .= Aeson.String "loop",
          "body" .= body
        ]

      If cond thenBody eifs elseB ->
        object [
          "type" .= Aeson.String "stmt",
          "kind" .= Aeson.String "if",
          "cond" .= cond,
          "then" .= thenBody,
          "elseifs" .= eifs,
          "else" .= elseB
        ]

      Local vdef ->
        object [
          "type" .= Aeson.String "stmt",
          "kind" .= Aeson.String "local",
          "def" .= vdef
        ]
      Set lvar expr ->
        object [
          "type" .= Aeson.String "stmt",
          "kind" .= Aeson.String "set",
          "lvar" .= lvar,
          "expr" .= expr
        ]
      Typedef ann ty1 ty2 ->
        object [
          "type" .= Aeson.String "toplevel",
          "kind" .= Aeson.String "typedef",
          "annotations" .= ann,
          "name" .= ty1,
          "base" .= ty2
        ]
      Global vdef ->
        object [
          "type" .= Aeson.String "toplevel",
          "kind" .= Aeson.String "global",
          "def" .= vdef
        ]
      Function ann constantness name params returnType body ->
        object [
          "type" .= Aeson.String "toplevel",
          "kind" .= Aeson.String "function",
          "annotations" .= ann,
          "constant" .= constantness,
          "name" .= name,
          "parameters" .= params,
          "returntype" .= returnType,
          "body" .= body
        ]
      Native ann constantness name params returnType ->
        object [
          "type" .= Aeson.String "toplevel",
          "kind" .= Aeson.String "native",
          "annotations" .= ann,
          "constant" .= constantness,
          "name" .= name,
          "parameters" .= params,
          "returntype" .= returnType
        ]

      Programm ts -> toJSON $ map toJSON ts

      

instance Compose (Ast a) where
    compose f a =
      case a of
        Programm toplvl -> Programm <$> traverse f toplvl
        Function ann c n a r body -> Function ann c n a r <$> traverse f body
        Global var -> Global <$> f var
        Set x y -> Set <$> f x <*> f y
        Local x -> Local <$> f x
        If e tb elseifs eb -> If <$> f e <*> traverse f tb <*> traverse composeEIf elseifs <*> traverse (traverse f) eb
          where
            composeEIf (cond, block) = (,) <$> f cond <*> traverse f block
        Loop b -> Loop <$> traverse f b
        Exitwhen cond -> Exitwhen <$> f cond
        Return (Just e) -> Return . Just <$> f e
        Call n args -> Call <$> pure n <*> traverse f args
        Var lvar -> Var <$> f lvar
        AVar n ix -> AVar <$> pure n <*> f ix
        SDef ann c n t (Just e) -> SDef ann c n t . Just <$> f e
        x -> pure x

