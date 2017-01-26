module Data.Dynamic where

import Data.Maybe (Maybe(..))
import Data.Typeable (class Typeable, TypeRep, typeOf)
import Prelude (class Show, otherwise, show, (<>), (==))
import Unsafe.Coerce (unsafeCoerce)

data Obj = Obj

data Dynamic = Dynamic TypeRep Obj

instance showDynamic :: Show Dynamic where
  show (Dynamic t _) = "<< " <> show t <> " >>"

toDynamic :: forall a. Typeable a => a -> Dynamic
toDynamic v = Dynamic (typeOf v) (unsafeCoerce v)

fromDynamic :: forall a. Typeable a => Dynamic -> Maybe a
fromDynamic (Dynamic t v) =
  case unsafeCoerce v of
    r | t == typeOf r -> Just r
      | otherwise     -> Nothing
