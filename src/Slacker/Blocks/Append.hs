{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Slacker.Blocks.Append
  ( type (++)
  , IxAppend(..)
  , (!>>)
  ) where

import           Data.Kind (Type)
import           Prelude hiding (Monad(..))

type family (++) (xs :: [Type]) (ys :: [Type]) where
  '[] ++ ys = ys
  (x ': xs) ++ ys = x ': (xs ++ ys)

-- | Can be used with QualifiedDo.
class IxAppend (m :: k -> Type -> Type) where
  type Unit m :: k
  type Plus m (i :: k) (j :: k) :: k
  (>>) :: m i a -> m j b -> m (Plus m i j) b

-- | Build up blocks and elements in sequence using this operator.
-- Alternatively, enable QualifiedDo and use do syntax to sequence blocks
-- and their elements.
(!>>) :: IxAppend m => m i a -> m j b -> m (Plus m i j) b
(!>>) = (>>)
