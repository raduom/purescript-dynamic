module Data.Dynamic
  ( Dynamic
  , toDynamic
  , fromDynamic
  ) where

import Data.Maybe (Maybe(..))
import Data.Typeable (class Typeable, TypeRep, typeOf)
import Prelude (class Show, otherwise, show, (<>), (==))
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

foreign import data Any :: Type

data Dynamic = Dynamic TypeRep Any

instance showDynamic :: Show Dynamic where
  show (Dynamic t _) = "<< " <> show t <> " >>"

toDynamic :: forall a. Typeable a => a -> Dynamic
toDynamic v = Dynamic (typeOf (Proxy :: Proxy a)) (unsafeCoerce v)

fromDynamic :: forall a. Typeable a => Dynamic -> Maybe a
fromDynamic (Dynamic t v)
  | t == typeOf (Proxy :: Proxy a) = Just (unsafeCoerce v)
  | otherwise = Nothing
