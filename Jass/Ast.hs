{-# LANGUAGE GADTs
  , StandaloneDeriving #-}


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


import Jass.Types

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

