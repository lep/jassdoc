{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Arrow
import Control.Monad

import qualified Data.ByteString.Lazy.Char8 as L8
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
    Native doc _ name _ _ -> L8.unlines [ delete name, handle doc name]
    Function doc _ name _ _ _ -> L8.unlines [ delete name, handle doc name]
    _ -> ""

  where
    delete name = L8.unlines [
          L8.unwords [ "delete from comments where fnname = ", t name, ";" ]
        , L8.unwords [ "delete from parameters where fnname = ", t name, ";" ]
        , L8.unwords [ "delete from annotations where fnname = ", t name, ";" ]
        ]

    handle (Just doc) name =
        let (descr, anns) = parseDocstring doc
            (params, annotations) = first (map $ extractParam . snd) $ split isParam anns
        in L8.unlines [ insertDescr name descr
                      , L8.unlines $ map (uncurry $ insertParam name) params
                      , L8.unlines $ map (uncurry $ insertAnn name) annotations
                      ]
    handle _ _ = ""

split pred xs = foldr ins mempty xs
  where
    ins elem (l, r)
        | pred elem = (elem:l, r)
        | otherwise = (l, elem:r)


extractParam xs =
    let name:descr = L8.words xs
    in (name, L8.unwords descr)

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


type Accumulator = ([L8.ByteString], [Maybe (L8.ByteString, L8.ByteString)])
parseDocstring = first (L8.unlines . reverse)
               . second (reverse . catMaybes)
               . flip go mempty
               . L8.lines
  where
    go :: [L8.ByteString] -> Accumulator -> Accumulator
    go [] acc = acc
    go (x:xs) (descr, anns)
        | emptyLine x = go xs (descr, Nothing:anns)
        | Just ann <- getAnn x = go xs (descr, Just ann:anns)
    go (x:xs) acc = go xs $ addToAnn x acc

    addToAnn :: L8.ByteString -> Accumulator -> Accumulator
    addToAnn text (descr, []) = (text:descr, [])
    addToAnn text (descr, Nothing:xs) = (text:descr, Nothing:xs)
    addToAnn text (descr, Just (ann, text'):xs) =
        (descr, Just (ann, (text' <> "\n" <> text)):xs)


    getAnn line =
        let words = map L8.unpack $ L8.words line
        in case words of
            ('@':ann):text -> Just (L8.pack ann, L8.pack $ unwords text)
            _ -> Nothing



emptyLine = L8.null . L8.filter (not . isVerticalSpace)
  where
    isVerticalSpace x = x `elem` [' ', '\t']


schema = L8.unlines
         [ " create table if not exists comments (    "
         , "      fnname text primary key,            "
         , "      comment text                        "
         , " );                                       "
         , " create table if not exists parameters (  "
         , "      fnname text,                        "
         , "      param text,                         "
         , "      value text,                         "
         , "      primary key (fnname, param)         "
         , " );                                       "
         , " create table if not exists annotations ( "
         , "      fnname text,                        "
         , "      anname text,                        "
         , "      value text                          "
         , " );                                       "
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

