{-# LANGUAGE  OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Annotation (Annotations(..), parseDocstring) where

import qualified Data.ByteString.Lazy.Char8 as L8
import Control.Arrow (second, Arrow ((***)))
import Data.Bifunctor (bimap)
import Data.Aeson (ToJSON (toJSON))
import Data.Text.Encoding (decodeUtf8Lenient)
import Data.ByteString.Lazy.Char8 (toStrict)

type Accumulator = ([L8.ByteString], [(L8.ByteString, [L8.ByteString])])

newtype Annotations = Annotations { getAnnotations :: [(L8.ByteString, L8.ByteString)] }
  deriving (Semigroup, Monoid)

instance ToJSON Annotations where
    toJSON (Annotations as) = toJSON $ map (x *** x) $ filter (not.L8.null.snd) as
      where
        x = decodeUtf8Lenient . toStrict

parseDocstring :: L8.ByteString -> Annotations
parseDocstring x =
  let (comment, annotations) = parseDocstring' x
  in Annotations $ ("comment", comment):annotations


parseDocstring' :: L8.ByteString
                  -> (L8.ByteString, [(L8.ByteString, L8.ByteString)])
parseDocstring' = bimap (trimWhitespace . myUnlines . reverse)
                       (map (second $ myUnlines . reverse ) )
               . flip go mempty
               . L8.lines
  where
    go :: [L8.ByteString] -> Accumulator -> Accumulator
    -- we reverse the annotations to have it inserted in the db in the same
    -- order they are written in the file. that way we can use sqlites _rowid_
    -- to query the annotations in the same order as they were written.
    -- this *might* only work for fresh builds as i don't know if sqlite
    -- recycles their internal rowids.
    go [] acc = second reverse acc 
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



trimWhitespace :: L8.ByteString -> L8.ByteString
trimWhitespace =
    {-  L8.reverse
    . L8.dropWhile (\x -> x=='\r' || x == '\n') -- drop back newline
    . L8.reverse
    . -} L8.dropWhile (\x -> x=='\r' || x == '\n') -- drop front newline

myUnlines :: [L8.ByteString] -> L8.ByteString
myUnlines = L8.intercalate "\n"
