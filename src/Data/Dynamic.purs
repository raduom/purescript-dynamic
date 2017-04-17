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

-- | The type used to encode dynamicaly typed objects
data Dynamic = Dynamic TypeRep Any

instance showDynamic :: Show Dynamic where
  show (Dynamic t _) = "<< " <> show t <> " >>"

-- | Converts an arbitrary value into an object of type `Dynamic`
toDynamic :: forall a. Typeable a => a -> Dynamic
toDynamic v = Dynamic (typeOf (Proxy :: Proxy a)) (unsafeCoerce v)

-- | Converts a `Dynamic` object back into an ordinary value of the
-- | correct type.
fromDynamic :: forall a. Typeable a => Dynamic -> Maybe a
fromDynamic (Dynamic t v)
  | t == typeOf (Proxy :: Proxy a) = Just (unsafeCoerce v)
  | otherwise = Nothing
