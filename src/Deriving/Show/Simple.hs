{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
module Deriving.Show.Simple (simpleShowsPrec, WrapSimple(..)) where

import Data.Coerce
import Data.Proxy
import Data.String
import Data.List (intersperse)
import GHC.Generics
import GHC.TypeLits
import qualified Data.HashMap.Strict as HM

simpleShowsPrec :: (Generic a, GShowSimple (Rep a)) => Int -> a -> ShowS
simpleShowsPrec prec a = showParen (prec > 10) $ foldr (.) id
  $ intersperse (showChar ' ')
  $ shows' (from a)

class GShowSimple f where
  shows' :: f x -> [ShowS]

instance GShowSimple V1 where
  shows' _ = error "impossible"

instance GShowSimple U1 where
  shows' _ = []

instance Show a => GShowSimple (K1 c a) where
  shows' (K1 a) = [showsPrec 11 a]

instance (GShowSimple f, GShowSimple g) => GShowSimple (f :*: g) where
  shows' (f :*: g) = shows' f ++ shows' g

instance (GShowSimple f, GShowSimple g) => GShowSimple (f :+: g) where
  shows' (L1 a) = shows' a
  shows' (R1 a) = shows' a

instance (KnownSymbol name, GShowSimple f, c ~ MetaCons name fix rec) => GShowSimple (C1 c f) where
  shows' (M1 a) = showString (symbolVal (Proxy :: Proxy name)) : shows' a

instance (GShowSimple f) => GShowSimple (D1 meta f) where
  shows' (M1 a) = shows' a

instance (GShowSimple f) => GShowSimple (S1 meta f) where
  shows' (M1 a) = shows' a

newtype WrapSimple a = WrapSimple { unwrapSimple :: a }

instance (Generic a, GShowSimple (Rep a)) => Show (WrapSimple a) where
  showsPrec d = simpleShowsPrec d . unwrapSimple
