{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Slacker.Blocks.Append
  ( type (++)
  , IxAppend(..)
  ) where

import           Data.Kind (Type)

type family (++) (xs :: [Type]) (ys :: [Type]) where
  '[] ++ ys = ys
  (x ': xs) ++ ys = x ': (xs ++ ys)

-- | Can be used with QualifiedDo.
class IxAppend (m :: k -> Type -> Type) where
  type Unit m :: k
  type Plus m (i :: k) (j :: k) :: k
  (>>) :: m i a -> m j b -> m (Plus m i j) b
