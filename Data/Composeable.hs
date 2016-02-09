{-# LANGUAGE RankNTypes #-}

module Data.Composeable
    ( Compose
    , compose
    , composeFold
    , composeM
    , composeOp
    , composeFoldM
    ) where

import Data.Monoid
import Control.Applicative
import Control.Monad.Identity
import Control.Monad.Writer

class Compose t where
    compose :: Applicative f => (forall a. t a -> f (t a)) -> t c -> f (t c)

composeFold :: (Monoid o, Compose t) => (forall a. t a -> o) -> t a -> o
composeFold f = getConst . compose (Const . f)

composeM :: (Monad m, Compose t) => (forall a. t a -> m (t a)) -> t a -> m (t a)
composeM f = unwrapMonad . compose (WrapMonad . f)

composeFoldM :: (Monoid o, Monad m, Compose t) => (forall a. t a -> m o) -> t a -> m o
composeFoldM f = execWriterT . unwrapMonad . compose t
  where
    t structure = WrapMonad $ do
        x <- lift $ f structure
        tell x
        --return undefined -- would be as valid
        return structure

composeOp :: Compose t => (forall k. t k -> t k) -> t k -> t k
composeOp f = runIdentity . compose (Identity . f)
