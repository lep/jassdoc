module Jass.Parser
    ( expression
    , statement
    , toplevel
    , programm

    , identifier
    , intlit
    , stringlit
    , reallit
    , rawcode

    , docstring
    , symbol

    ) where

import Control.Applicative hiding (many, some)
import Control.Monad

import Data.Maybe
import Data.Void
import Data.Functor( ($>))

import Jass.Ast
import Jass.Types

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import Control.Monad.Combinators.Expr

import Annotation
import qualified Data.ByteString.Lazy.Char8 as L8

type Parser = Parsec Void String

sc = L.space sc' lc empty
  where
    sc' =void $ takeWhile1P (Just "white space") (\w -> w == ' ' || w == '\t')
    lc = L.skipLineComment "//"


lexeme = L.lexeme sc

stringlit = lexeme $ char '"' >> manyTill L.charLiteral (char '"')

rawcode :: Parser String
rawcode = lexeme $ char '\'' *> (escaped <|> anyCC)
  where
    escaped = do
       char '\\'
       c <- oneOf ("btrnf" :: String)
       char '\''
       return ['\\', c]
    anyCC = manyTill anySingle (char '\'')

intlit :: Parser String
intlit = try $ do
    n <- option "" $ string "-"
    int <- show <$> lexeme (try hexlit <|> try octlit <|> L.decimal)
    return $ n <> int

hexlit = char '$' *> L.hexadecimal
     <|> char '0' *> (char 'X' <|> char 'x') *> L.hexadecimal
octlit = char '0' *> L.octal


reallit = try $ do
    n <- option "" $ string "-"
    r <- lexeme $ dotReal <|> realDot
    return $ n <> r
dotReal = do
    char '.'
    a <- some digitChar
    return $ "0." <> a
realDot = do
    a <- some digitChar
    char '.'
    b <- many digitChar
    return $ a <> "." <> b


symbol = L.symbol sc

reserved :: String -> Parser ()
reserved w = (lexeme . try) (string w *> notFollowedBy alphaNumChar)

identifier :: Parser String
identifier = (lexeme . try) (p >>= check)
  where
    p = (:) <$> letterChar <*> many (alphaNumChar <|> char '_')
    check x = if x `elem` keywords
            then fail $ "keyword " ++ show x ++ " cannot be an identifier"
            else return x
    keywords = [ "globals", "endglobals", "if", "then", "elseif", "else"
               , "endif", "loop", "endloop", "set", "call", "return"
               , "takes", "returns", "constant", "native", "function"
               , "nothing", "true", "false", "null", "and", "or", "not"
               , "local", "array"
               ]

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")
brackets = between (symbol "[") (symbol "]")


horizontalSpace = void $ some $ lexeme $ optional (L.skipLineComment "//") *> eol

docstring :: Parser Annotations
docstring = option mempty $ do
    symbol "/**"
    anns <- parseDocstring . L8.pack <$> manyTill anySingle (symbol "*/")
    horizontalSpace
    pure anns

toplevel :: Parser [Ast Annotations Toplevel]
toplevel = globals
        <|> doc'd

  where
    doc'd :: Parser [Ast Annotations Toplevel]
    doc'd = do
        doc <- docstring
        functionLike doc <|> typedef doc

    functionLike :: Annotations -> Parser [Ast Annotations Toplevel]
    functionLike doc = do
        const <- fromMaybe Normal <$> optional (reserved "constant" $> Jass.Types.Const)
        native doc const <|> function doc const

    globals :: Parser [Ast Annotations Toplevel]
    globals = between (reserved "globals" <* horizontalSpace)
                      (reserved "endglobals" <* horizontalSpace) $ many $ do
        doc <-  docstring
        const <- fromMaybe Normal <$> optional (reserved "constant" $> Jass.Types.Const)
        vdecl <- vardecl doc const
        return $ Global vdecl

    typedef :: Annotations -> Parser [Ast Annotations Toplevel]
    typedef doc = do
        reserved "type"
        new <- identifier
        reserved "extends"
        base <- identifier
        horizontalSpace
        return [Typedef doc new base]


pSignature = do
    name <- identifier
    reserved "takes"
    args <- (reserved "nothing" $> []) <|> ((,) <$> identifier <*> identifier) `sepBy` symbol ","
    reserved "returns"
    ret <- (reserved "nothing" $> "nothing") <|> identifier
    horizontalSpace
    return (name, args, ret)

native :: Annotations -> Constant -> Parser [Ast Annotations Toplevel]
native doc const = do
    reserved "native"
    (name, args, ret) <- pSignature
    return [Native doc const name args ret]

function :: Annotations -> Constant -> Parser [Ast Annotations Toplevel]
function doc const = do
    reserved "function"
    (name, args, ret) <- pSignature
    body <- many statement
    reserved "endfunction"
    horizontalSpace
    return [Function doc const name args ret body]

statement :: Parser (Ast Annotations Stmt)
statement = returnStmt
          <|> if_
          <|> callStmt
          <|> loop
          <|> set
          <|> exitwhen
          <|> local
          <?> "statement"
    where
        local = Local <$> (reserved "local"*> vardecl mempty Normal)
        returnStmt = Return <$> (reserved "return" *> optional expression <* horizontalSpace)
        callStmt = Call <$> (optional (reserved "debug") *> reserved "call" *> identifier) <*> parens arglist <* horizontalSpace
        loop = Loop <$> between startLoop endLoop (many statement)

        set = Set <$> (reserved "set" *> lvar)
                  <*> (symbol "=" *> expression)
                  <* horizontalSpace
        lvar = do
            v <- identifier
            arr <- optional $ brackets expression
            case arr of
                Just idx -> return $ AVar v idx
                Nothing -> return $ SVar v


        exitwhen = Exitwhen <$> (reserved "exitwhen" *> expression <* horizontalSpace)

        if_ = If <$> (reserved "if" *> expression <* reserved "then" <* horizontalSpace)
                 <*> many statement
                 <*> many elseif
                 <*> optional else_
                 <*  reserved "endif" <* horizontalSpace


        elseif =
            (,) <$> (reserved "elseif" *> expression)
                <*> (reserved "then" >> horizontalSpace *> many statement)


        else_ = reserved "else" *> horizontalSpace *> many statement

        startLoop = reserved "loop" <* horizontalSpace
        endLoop = reserved "endloop" <* horizontalSpace

vardecl :: Annotations -> Constant -> Parser (Ast Annotations VarDef)
vardecl doc constantness = do
    typ <- identifier
    isArray <- (reserved "array" $> True) <|> pure False
    if isArray
    then varArray doc typ <* horizontalSpace
    else varNormal doc typ <* horizontalSpace

  where
    varArray doc typ = ADef doc <$> identifier <*> pure typ
    varNormal doc typ = SDef doc constantness <$> identifier <*> pure typ <*> optional (symbol "=" *> expression)

expression = makeExprParser term table
            <?> "expression"
  where
    table = [ [ binary (symbol "%") "%", binary (symbol "*") "*", binary (symbol "/") "/"]
            , [ binary (symbol "+") "+", binary (symbol "-") "-"]
            , zipWith binary
                [symbol "<=", symbol "<" , symbol ">=", symbol ">", symbol "!=" , symbol "==" ]
                ["<="       , "<"        , ">="   , ">"      , "!="   , "==" ]
            , [ binary (reserved "or") "or"]
            , [ binary (reserved "and") "and"]
            ]
    binary t op = InfixL (t $> (\a b -> Call op [a, b]))

term = parens expression
    <|> literal
    <|> varOrCall
    <|> symbol "+"      *> ((\e -> Call "+" [e]) <$> term)
    <|> symbol "-"      *> ((\e -> Call "-" [e]) <$> term)
    <|> reserved "not"  *> ((\e -> Call "not" [e]) <$> expression)
    <?> "term"
  where
    literal = String <$> stringlit
            <|> either Real Int <$> eitherP (try reallit) intlit
            <|> Rawcode <$> rawcode
            <|> (reserved "true" $> Bool True)
            <|> (reserved "false" $> Bool False)
            <|> (reserved "null" $> Null)
            <|> Code <$> (reserved "function" *> identifier)

    varOrCall = do
        name <- identifier
        choice [ c name, a name, v name ]

    c name = Call name <$> parens arglist
    a name = Var . AVar name <$> brackets expression
    v name = return . Var $ SVar name

arglist = expression `sepBy` symbol ","

programm :: Parser (Ast Annotations Programm)
programm = Programm . concat <$> (many horizontalSpace *> many toplevel <* eof)

