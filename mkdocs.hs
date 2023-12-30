{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PatternSynonyms #-}

import Data.Bifunctor (bimap)

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

import Text.Megaparsec (parse, errorBundlePretty)

import Options.Applicative

import Annotation


pattern N x <- (L8.pack -> x)
pattern P x <- (fmap L8.pack -> x)
pattern P2 x <- (fmap (L8.pack***L8.pack) -> x)

handleToplevel :: FilePath -> Ast Annotations Toplevel -> L8.ByteString
handleToplevel file toplevel =
  case toplevel of
    Typedef doc (N name) _ -> L8.unlines [ delete name, handle doc name, attachFile file name]
    Native doc _ (N name) (P2 params) r ->
      L8.unlines [ delete name
                 , handle doc name
                 , paramOrdering name params
                 , attachFile file name
                 , returnType name r
                 ]
    Function doc _ (N name) (P2 params) r _ ->
      L8.unlines [ delete name
                 , handle doc name
                 , paramOrdering name params
                 , attachFile file name
                 , returnType name r
                 ]
    Global (ADef doc (N name) (N ty)) -> L8.unlines
        [ delete name
        , handle doc name
        , attachFile file name
        ]
    Global (SDef doc isConst (N name) (N ty) _) -> L8.unlines
        [ delete name
        , handle doc name
        , attachFile file name
        ]
    _ -> ""

  where
    delete name = L8.unlines [
          L8.unwords [ "delete from parameters where fnname = ", t name, ";" ]
        , L8.unwords [ "delete from annotations where fnname = ", t name, ";" ]
        , L8.unwords [ "delete from params_extra where fnname = ", t name, ";" ]
        ]


    handle (Annotations anns) name =
        let (params, annotations) = first (map $ extractParam . snd) $ split isParam anns
        in L8.unlines [ L8.unlines $ map (uncurry $ insertParam name) params
                      , L8.unlines $ map (uncurry $ insertAnn name) annotations
                      ]

    returnType name (L8.pack -> r) =
        L8.unwords [ "insert into annotations values("
                   , t name, ","
                   , t "return-type", ","
                   , t r
                   , ");"
                   ]

    attachFile (L8.pack -> file) name =
        L8.unwords [ "insert into annotations values("
                   , t name, ","
                   , t "source-file", ","
                   , t file
                   , ");"
                   ]

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

split :: (a -> Bool) -> [a] -> ([a], [a])
split pred = foldr ins mempty
  where
    ins elem (l, r)
        | pred elem = (elem:l, r)
        | otherwise = (l, elem:r)


extractParam xs =
    let (name, descr) = L8.break isSpace xs
    in (name, L8.dropWhile isSpace descr)

isParam = ("param" == ) . fst

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


chop = L8.dropWhileEnd (`elem` ['\r', '\n'])

t x = "'" <> escape (chop x) <> "'"
escape = L8.pack . concatMap e . L8.unpack
e '\'' = "''"
e x    = [x]

emptyLine = L8.null . L8.filter (not . isVerticalSpace)
  where
    isVerticalSpace x = x `elem` [' ', '\t']


schema = L8.unlines
         [ " create table if not exists parameters (      "
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

         , " create index if not exists annotation_index  "
         , " on annotations(fnname);                      "

         , " create table if not exists metadata (        "
         , "    key text primary key,                     "
         , "    value text                                "
         , ");                                            "
         ]

data Args = Args FilePath [FilePath]

argsParser :: Parser Args
argsParser =
    Args <$> strOption (long "output" <> metavar "FILE" <> help "Write output to FILE")
         <*> many (argument str (metavar "INPUTS..."))

opts :: ParserInfo Args
opts = info (argsParser <**> helper)
    ( fullDesc
    <> progDesc "Process jassdoc'd jass files"
    )

main = do
    Args out args <- execParser opts
    h <- openFile out WriteMode
    L8.hPutStrLn h schema
    forM_ args $ \file -> do
        hPutStrLn stderr file
        handle <- openFile file ReadMode
        hSetBinaryMode handle True
        x <- parse programm file <$> hGetContents handle -- closes handle
        let toplevel = case x of
                Right (Programm y) -> y
                Left err -> error $ errorBundlePretty err
        L8.hPutStrLn h "BEGIN TRANSACTION;"
        mapM_ (L8.hPutStrLn h) . filter (not . emptyLine) $ map (handleToplevel file) toplevel
        L8.hPutStrLn h "END TRANSACTION;"
    hClose h

