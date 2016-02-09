{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Jass.Parser
    ( expression
    , statement
    , toplevel
    , programm
    
    , Jass.Parser.parse
    ) where

import Control.Applicative
import Control.Monad

import qualified Data.ByteString.Lazy as BL
import Data.Char
import Data.Maybe
import Data.Monoid

import Jass.Ast
import Jass.Tokenizer (Token)
import qualified Jass.Tokenizer as Tok
import Jass.Types

import Text.Megaparsec
import Text.Megaparsec.Expr
import Text.Megaparsec.Pos
import Text.Megaparsec.ShowToken


newtype TokenStream = TokenStream [TokenPos]
    deriving (Show)

data TokenPos = TokenPos Token Tok.AlexPosn
    deriving (Show)

instance Stream TokenStream TokenPos where
    uncons (TokenStream []) = Nothing
    uncons (TokenStream (x:xs)) = Just (x, TokenStream xs)

instance StorableStream TokenStream TokenPos where
    fromFile path = TokenStream . map (uncurry TokenPos) . Tok.alexScanTokens <$> BL.readFile path

instance ShowToken TokenPos where
    showToken = show

instance ShowToken [TokenPos] where
    showToken = show

alexPosnToSourcePos (Tok.AlexPn _ l c) = newPos "" l c

tokenToSourcePos _ _ (TokenPos _ pos) = alexPosnToSourcePos pos


tok ::Token -> Parsec TokenStream TokenPos
tok t = token tokenToSourcePos p
  where
    p (TokenPos t' _) | t' == t = Right $ TokenPos t undefined
    p t' = Left . pure . Unexpected $ showToken t'

ident :: Parsec TokenStream BL.ByteString
ident = token tokenToSourcePos p
  where
    p (TokenPos (Tok.Id i) _) = Right i
    p t          = Left . pure . Unexpected $ showToken t

stringlit :: Parsec TokenStream BL.ByteString
stringlit = token tokenToSourcePos p
  where
    p (TokenPos (Tok.String s) _) = Right s
    p t              = Left . pure . Unexpected $ showToken t

intlit = token tokenToSourcePos p
  where
    p (TokenPos (Tok.Intlit i) _) = Right i
    p t              = Left . pure . Unexpected $ showToken t

reallit = token tokenToSourcePos p
  where
    p (TokenPos (Tok.Reallit r) _) = Right r
    p t               = Left . pure . Unexpected $ showToken t

rawcode = token tokenToSourcePos p
  where
    p (TokenPos (Tok.Rawcode r) _) = Right r
    p t               = Left . pure . Unexpected $ showToken t

docstring = token tokenToSourcePos p
  where
    p (TokenPos (Tok.Doc s) _) = Right s
    p t                        = Left . pure . Unexpected $ showToken t



identifier = ident

parens = between (tok Tok.LParen) (tok Tok.RParen)
brackets = between (tok Tok.LBracket) (tok Tok.RBracket)

horizontalSpace = some $ tok Tok.Newline

toplevel = globals
        <|> doc'd

  where
    doc'd = do
        doc <- optional (docstring <* horizontalSpace)
        functionLike doc <|> typedef doc
    functionLike doc = do
        const <- fromMaybe Normal <$> optional (tok Tok.Constant *> pure Jass.Types.Const)
        native doc const <|> function doc const

    globals = between (tok Tok.Globals <* horizontalSpace)
                      (tok Tok.Endglobals <* horizontalSpace) $ many $ do
        const <- fromMaybe Normal <$> optional (tok Tok.Constant *> pure Jass.Types.Const)
        vdecl <- vardecl const
        return $ Global vdecl

    typedef doc = do
        tok Tok.Type
        new <- identifier
        tok Tok.Extends
        base <- identifier
        horizontalSpace
        return [Typedef doc new base]


native doc const = do
    tok Tok.Native
    name <- identifier
    tok Tok.Takes
    args <- (tok Tok.Nothin *> pure []) <|> ((,) <$> identifier <*> identifier) `sepBy` (tok Tok.Comma)
    tok Tok.Returns
    ret <- (tok Tok.Nothin *> pure "nothing") <|> identifier
    horizontalSpace
    return [Native doc const name args ret]

function doc const = do
    tok Tok.Function
    name <- identifier
    tok Tok.Takes
    args <- (tok Tok.Nothin *> pure []) <|> ((,) <$> identifier <*> identifier) `sepBy` (tok Tok.Comma)
    tok Tok.Returns
    ret <- (tok Tok.Nothin *> pure "nothing") <|> identifier
    horizontalSpace
    body <- many statement
    tok Tok.Endfunction
    horizontalSpace
    return [Function doc const name args ret body]

statement = returnStmt
          <|> if_
          <|> callStmt
          <|> loop
          <|> set
          <|> exitwhen
          <|> local
          <?> "statement"
    where
        local = Local <$> (tok Tok.Local *> vardecl Normal)
        returnStmt = Return <$> (tok Tok.Return *> optional expression <* horizontalSpace)
        callStmt = Call <$> (tok Tok.Call *> identifier) <*> parens arglist <* horizontalSpace
        loop = Loop <$> between startLoop endLoop (many statement)

        set = Set <$> (tok Tok.Set *> lvar)
                  <*> (tok Tok.Equal *> expression)
                  <* horizontalSpace
        lvar = do
            v <- identifier
            arr <- optional $ brackets expression
            case arr of
                Just idx -> return $ AVar v idx
                Nothing -> return $ SVar v

            
        exitwhen = Exitwhen <$> (tok Tok.Exitwhen *> expression <* horizontalSpace)

        if_ = If <$> (tok Tok.If *> expression <* tok Tok.Then <* horizontalSpace)
                 <*> (many statement)
                 <*> many elseif
                 <*> optional else_
                 <*  tok Tok.Endif <* horizontalSpace


        elseif =
            (,) <$> (tok Tok.Elseif *> expression)
                <*> (tok Tok.Then >> horizontalSpace *> many statement)


        else_ = tok Tok.Else *> horizontalSpace *> many statement

        startLoop = tok Tok.Loop <* horizontalSpace
        endLoop = tok Tok.Endloop <* horizontalSpace

vardecl constantness = do
    typ <- identifier
    isArray <- tok Tok.Array *> pure True <|> pure False
    if isArray
    then varArray typ <* horizontalSpace
    else varNormal typ <* horizontalSpace

  where
    varArray typ = ADef <$> identifier <*> pure typ
    varNormal typ = SDef constantness <$> identifier <*> pure typ <*> optional (tok Tok.Equal *> expression)

expression = makeExprParser term table
            <?> "expression"
  where
    table = [ [ binary Tok.Mult "*", binary Tok.Div "/"]
            , [ binary Tok.Plus "+", binary Tok.Minus "-"]
            , zipWith binary
                [Tok.LEQ, Tok.LTtok, Tok.GEQ, Tok.GTtok, Tok.NEQ, Tok.EQtok]
                ["<="   , "<"      , ">="   , ">"      , "!="   , "==" ]
            , [binary Tok.And "and", binary Tok.Or "or"]
            ]
    unary t op = Prefix (tok t *> pure (\e -> Call op [e]))
    binary t op = InfixL (tok t *> pure (\a b -> Call op [a, b]))

term = parens expression
    <|> tok Tok.Not   *> (((\e -> Call "not" [e])) <$> expression)
    <|> tok Tok.Minus *> (((\e -> Call "-" [e])) <$> expression)
    <|> tok Tok.Plus  *> (((\e -> Call "-" [e])) <$> expression)
    <|> literal
    <|> varOrCall
    <?> "term"
  where
    literal = String <$> stringlit
            <|> Int <$> intlit
            <|> Real <$> reallit
            <|> Rawcode <$> rawcode
            <|> (tok Tok.FALSE *> pure ( Bool True))
            <|> (tok Tok.TRUE *> pure ( Bool False))
            <|> (tok Tok.NULL *> pure Null)
            <|> Code <$> (tok Tok.Function *> identifier)

    varOrCall = do
        name <- identifier
        choice [ c name, a name, v name ]

    c name = Call name <$> parens arglist
    a name = Var . AVar name <$> brackets expression
    v name = return . Var $ SVar name

arglist = expression `sepBy` tok Tok.Comma

programm = Programm . concat <$> (many horizontalSpace *> many toplevel <* eof)


parse :: Parsec TokenStream a -> BL.ByteString -> Either ParseError a
parse parser = Text.Megaparsec.parse parser ""
                . TokenStream . map (uncurry TokenPos) . Tok.alexScanTokens
