{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Arrow
import Control.Monad

import qualified Data.ByteString.Lazy.Char8 as L8

import Data.Char (isSpace)
import Data.Maybe (catMaybes, mapMaybe)
import Data.Monoid

import Jass.Parser
import Jass.Ast
import Jass.Types


import System.Environment (getArgs)
import System.IO


handleToplevel :: Ast (Maybe L8.ByteString) Toplevel -> L8.ByteString
handleToplevel toplevel =
  case toplevel of
    Typedef doc name _ -> L8.unlines [ delete name, handle doc name]
    Native doc _ name params _ ->
      L8.unlines [ delete name
                 , handle doc name
                 , paramOrdering name params
                 ]
    Function doc _ name params _ _ ->
      L8.unlines [ delete name
                 , handle doc name
                 , paramOrdering name params
                 ]
    _ -> ""

  where
    delete name = L8.unlines [
          L8.unwords [ "delete from comments where fnname = ", t name, ";" ]
        , L8.unwords [ "delete from parameters where fnname = ", t name, ";" ]
        , L8.unwords [ "delete from annotations where fnname = ", t name, ";" ]
        , L8.unwords [ "delete from params_extra where fnname = ", t name, ";" ]
        ]

    handle (Just doc) name =
        let (descr, anns) = parseDocstring doc
            (params, annotations) = first (map $ extractParam . snd) $ split isParam anns
        in L8.unlines [ insertDescr name descr
                      , L8.unlines $ map (uncurry $ insertParam name) params
                      , L8.unlines $ map (uncurry $ insertAnn name) annotations
                      ]
    handle _ _ = ""

    paramOrdering name params = L8.unlines $ zipWith (oneParam name) params [1..]
    oneParam name (typ, param) idx = L8.unlines [
       L8.unwords [ "insert into params_extra values ("
                  , t name, ","
                  , t param, ","
                  , t "param_order", ","
                  , L8.pack $ show idx
                  , ");"
                  ]
     , L8.unwords [ "insert into params_extra values ("
                  , t name, ","
                  , t param, ","
                  , t "param_type", ","
                  , t typ
                  , ");"
                  ]
     ]

split pred xs = foldr ins mempty xs
  where
    ins elem (l, r)
        | pred elem = (elem:l, r)
        | otherwise = (l, elem:r)


extractParam xs =
    let (name, descr) = L8.break isSpace xs
    in (name, L8.dropWhile isSpace descr)

isParam = ("param" == ) . fst

insertDescr name descr = L8.unwords
    [ "insert into comments (fnname, comment) values ("
    , L8.intercalate "," [t name, t descr]
    , ");"
    ]

insertParam name param value = L8.unwords
    [ "insert into parameters (fnname, param, value) values ("
    , L8.intercalate "," [t name, t param, t value ]
    , ");"
    ]

insertAnn name ann value = L8.unwords
    [ "insert into annotations (fnname, anname, value) values ("
    , L8.intercalate "," [t name, t ann, t value ]
    , ");"
    ]


t x = "'" <> escape x <> "'"
escape = L8.pack . concatMap e . L8.unpack
e '\'' = "''"
e x    = [x]


trimWhitespace =
    {-  L8.reverse
    . L8.dropWhile (\x -> x=='\r' || x == '\n') -- drop back newline
    . L8.reverse
    . -} L8.dropWhile (\x -> x=='\r' || x == '\n') -- drop front newline

type Accumulator = ([L8.ByteString], [(L8.ByteString, [L8.ByteString])])


myUnlines = L8.intercalate "\n"


parseDocstring = first (trimWhitespace . myUnlines . reverse)
               . second (map (second $ myUnlines . reverse ) )
               . flip go mempty
               . L8.lines
  where
    go :: [L8.ByteString] -> Accumulator -> Accumulator
    go [] acc = acc
    go (x:xs) (descr, annotations)
        | Just ann <- getAnn x = go xs (descr, ann:annotations)
        | null annotations = go xs (x:descr, annotations)
        | otherwise =
            let (anTag, anLines):as = annotations
            in go xs (descr, (anTag, x:anLines):as)



    getAnn line =
        let words = map L8.unpack $ L8.words line
        in case words of
            ('@':ann):text -> Just (L8.pack ann, [trimWhitespace . L8.pack $ unwords text])
            _ -> Nothing



emptyLine = L8.null . L8.filter (not . isVerticalSpace)
  where
    isVerticalSpace x = x `elem` [' ', '\t']


schema = L8.unlines
         [ " create table if not exists comments (        "
         , "      fnname text primary key,                "
         , "      comment text                            "
         , " );                                           "
         , " create table if not exists parameters (      "
         , "      fnname text,                            "
         , "      param text,                             "
         , "      value text,                             "
         , "      primary key (fnname, param)             "
         , " );                                           "
         , " create table if not exists annotations (     "
         , "      fnname text,                            "
         , "      anname text,                            "
         , "      value text                              "
         , " );                                           "
         , " create table if not exists params_extra (    "
         , "      fnname text,                            "
         , "      param text,                             "
         , "      anname text,                            "
         , "      value,                                  "
         , "      primary key (fnname, param, anname)     "
         , " );                                           "
         ]

main = do
    args <- getArgs
    L8.putStrLn schema
    forM_ args $ \file -> do
        hPutStrLn stderr file
        Right (Programm toplevel) <- parse programm <$> L8.readFile file
        L8.putStrLn "BEGIN TRANSACTION;"
        mapM_ L8.putStrLn . filter (not . emptyLine) $ map handleToplevel toplevel
        L8.putStrLn "END TRANSACTION;"

