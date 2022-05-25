{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PatternSynonyms #-}

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


pattern N x <- (L8.pack -> x)
pattern P x <- (fmap L8.pack -> x)
pattern P2 x <- (fmap (L8.pack***L8.pack) -> x)

handleToplevel :: FilePath -> Ast (Maybe String) Toplevel -> L8.ByteString
handleToplevel file toplevel =
  case toplevel of
    Typedef (P doc) (N name) _ -> L8.unlines [ delete name, handle doc name, attachFile file name]
    Native (P doc) _ (N name) (P2 params) r ->
      L8.unlines [ delete name
                 , handle doc name
                 , paramOrdering name params
                 , attachFile file name
                 , returnType name r
                 ]
    Function (P doc) _ (N name) (P2 params) r _ ->
      L8.unlines [ delete name
                 , handle doc name
                 , paramOrdering name params
                 , attachFile file name
                 , returnType name r
                 ]
    Global (ADef (P doc) (N name) (N ty)) -> L8.unlines
        [ delete name
        , handle doc name
        , attachFile file name
        ]
    Global (SDef (P doc) isConst (N name) (N ty) _) -> L8.unlines
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


    handle (Just doc) name =
        let (descr, anns) = parseDocstring doc
            (params, annotations) = first (map $ extractParam . snd) $ split isParam anns
        in L8.unlines [ insertDescr name descr
                      , L8.unlines $ map (uncurry $ insertParam name) params
                      , L8.unlines $ map (uncurry $ insertAnn name) annotations
                      ]
    handle _ _ = ""

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

split pred xs = foldr ins mempty xs
  where
    ins elem (l, r)
        | pred elem = (elem:l, r)
        | otherwise = (l, elem:r)


extractParam xs =
    let (name, descr) = L8.break isSpace xs
    in (name, L8.dropWhile isSpace descr)

isParam = ("param" == ) . fst

insertDescr name descr
  | L8.null descr = L8.empty
  | otherwise = L8.unwords
    [ "insert into annotations (fnname, anname, value) values ("
    , L8.intercalate "," [t name, t "comment", t descr]
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

main = do
    args <- getArgs
    L8.putStrLn schema
    forM_ args $ \file -> do
        hPutStrLn stderr file
        handle <- openFile file ReadMode
        hSetBinaryMode handle True
        x <- parse programm file <$> hGetContents handle -- closes handle
        let toplevel :: [Ast (Maybe String) Toplevel]
            toplevel = case x of
                Right (Programm y) -> y
                Left err -> error $ errorBundlePretty err
        L8.putStrLn "BEGIN TRANSACTION;"
        mapM_ L8.putStrLn . filter (not . emptyLine) $ map (handleToplevel file) toplevel
        L8.putStrLn "END TRANSACTION;"

