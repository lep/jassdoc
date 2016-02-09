{
{-# LANGUAGE TupleSections #-}
module Jass.Tokenizer where

import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BS

}

%wrapper "posn-bytestring"

@nl = ((\r*\n)|\r)+
$digit = 0-9
$hexdigit = [0-9 A-F a-f]
$octdigit = 0-7

@hexstart = \$ | 0x | 0X

$idStart = [ a-z A-Z ]
$idMiddle = [ 0-9 a-z A-Z _]
$idEnd = [ 0-9 a-z A-Z ]
@id = $idStart ( $idMiddle* $idEnd)?

@notStar = . # \*
@notSlash = . # \/
@notSlashOrStar = . # \/ # \*

@validDoc = (@notStar \/) | (\* @notSlash) | @notSlashOrStar



@notquote_string = [^"\\]|@nl
@escape_string = \\[btnrf\\"]
@string = \" (@escape_string | @notquote_string)* \"

@notquote_raw = [^\'\\]|@nl
@escape_raw = \\[btnrf\'\\]
@rawcode = ' (@escape_raw | @notquote_raw)* '

tokens :-

"/**"(@validDoc|@nl)+"*/"   { \pos s -> (Doc . BS.drop 3 . BS.reverse . BS.drop 2 $ BS.reverse s, pos) }


"//".*@nl  { \pos s -> (Newline, pos) }

@nl        { \pos s -> (Newline, pos) }
$white   ;


@string     { \pos s -> (String . BS.init $ BS.tail s, pos) }
@rawcode     { \pos s -> (Rawcode s, pos) }

$digit+"."$digit*   { \pos s -> (Reallit s, pos) }
$digit*"."$digit+   { \pos s -> (Reallit s, pos) }
@hexstart$hexdigit+  { \pos s -> (Intlit s, pos) }
$digit+             { \pos s -> (Intlit s, pos) }

"if"            { const . (If, ) }
"then"          { const . (Then, ) }
"else"          { const . (Else, ) }
"elseif"        { const . (Elseif, ) }
"endif"         { const . (Endif, ) }

"loop"          { const . (Loop, ) }
"exitwhen"      { const . (Exitwhen, ) }
"endloop"       { const . (Endloop, ) }

"globals"       { const . (Globals, ) }
"endglobals"    { const . (Endglobals, ) }

"constant"      { const . (Constant, ) }
"native"        { const . (Native, ) }
"function"      { const . (Function, ) }
"takes"         { const . (Takes, ) }
"returns"       { const . (Returns, ) }
"endfunction"   { const . (Endfunction, ) }
"nothing"       { const . (Nothin, ) }

"not"       { const . (Not, ) }
"type"       { const . (Type, ) }
"extends"       { const . (Extends, ) }
"local"       { const . (Local, ) }
"array"       { const . (Array, ) }
"set"       { const . (Set, ) }
"call"       { const . (Call, ) }
"return"       { const . (Return, ) }
"true"       { const . (TRUE, ) }
"false"       { const . (FALSE, ) }
"null"       { const . (NULL, ) }
"and"       { const . (And, ) }
"or"       { const . (Or, ) }
","       { const . (Comma, ) }
"="       { const . (Equal, ) }
"*"       { const . (Mult, ) }
"+"       { const . (Plus, ) }
"/"       { const . (Div, ) }
"-"       { const . (Minus, ) }
"("       { const . (LParen, ) }
")"       { const . (RParen, ) }
"["       { const . (LBracket, ) }
"]"       { const . (RBracket, ) }
"<="       { const . (LEQ, ) }
"<"       { const . (LTtok, ) }
">="       { const . (GEQ, ) }
">"       { const . (GTtok, ) }
"=="       { const . (EQtok, ) }
"!="       { const . (NEQ, ) }

@id        { \pos s -> (Id s, pos) }



{
data Token = Rawcode ByteString
           | String ByteString
           | Reallit ByteString
           | Intlit ByteString
           | Id ByteString
           | Doc ByteString
           | If
           | Then
           | Else
           | Elseif
           | Endif
           | Globals
           | Endglobals
           | Loop
           | Endloop
           | Exitwhen
           | Nothin
           | Takes
           | Returns
           | Native
           | Function
           | Endfunction
           | Newline
           | Constant
           | NEQ
           | GEQ
           | LEQ
           | RBracket
           | LBracket
           | RParen
           | LParen
           | Minus
           | Div
           | Plus
           | Mult
           | Equal
           | Or
           | And
           | NULL
           | FALSE
           | TRUE
           | Return
           | Call
           | Array
           | Local
           | Set
           | Type
           | Extends
           | Comma
           | Not
           | LTtok
           | GTtok
           | EQtok
           deriving (Eq, Show)

getId (Id n) = n
getId (Intlit n) = n
getId (Reallit n) = n
getId (Rawcode n) = n
getId (String n) = n

--main = do
--    s <- BS.getContents
--    print $ alexScanTokens s
}
