{-# LANGUAGE OverloadedStrings, NoMonomorphismRestriction, FlexibleContexts, MultiParamTypeClasses, ScopedTypeVariables, DeriveDataTypeable, DeriveGeneric, GADTs, StandaloneDeriving #-}
module EZCouch.View where

import Prelude ()
import ClassyPrelude
import GHC.Generics
import Data.Aeson
import EZCouch.Action
import EZCouch.Entity
import EZCouch.Types
import EZCouch.Design 
import EZCouch.WriteAction
import qualified Control.Monad as Monad
import qualified Data.Foldable as Foldable
import qualified EZCouch.Model.Design as DesignModel
import qualified EZCouch.Model.View as ViewModel
import qualified EZCouch.Base62 as Base62
import Data.Hashable 
import EZCouch.JS

type ViewModel = ViewModel.View
type DesignModel = DesignModel.Design


data ViewKey a = 
  ViewKeyField Text |
  ViewKeyRandom
  deriving (Show, Eq)


instance ToJS (ViewKey a) where
  toJS (ViewKeyField field) = "doc." ++ field
  toJS ViewKeyRandom = "Math.random()"
instance Hashable (ViewKey a) where
  hashWithSalt salt = hashWithSalt salt . toJS

-- TODO: rename ViewById and ViewByKeyN
data View entity keys where
  ViewById 
    :: View entity Text
  ViewByKeys1 
    :: ViewKey a 
    -> View entity a
  ViewByKeys2 
    :: ViewKey a 
    -> ViewKey b 
    -> View entity (a, b)
  ViewByKeys3 
    :: ViewKey a 
    -> ViewKey b 
    -> ViewKey c 
    -> View entity (a, b, c)
  ViewByKeys4 
    :: ViewKey a 
    -> ViewKey b 
    -> ViewKey c 
    -> ViewKey d 
    -> View entity (a, b, c, d)
  ViewByKeys5 
    :: ViewKey a 
    -> ViewKey b 
    -> ViewKey c 
    -> ViewKey d 
    -> ViewKey e 
    -> View entity (a, b, c, d, e)
  ViewByKeys6 
    :: ViewKey a 
    -> ViewKey b 
    -> ViewKey c 
    -> ViewKey d 
    -> ViewKey e 
    -> ViewKey f 
    -> View entity (a, b, c, d, e, f)
  ViewByKeys7 
    :: ViewKey a 
    -> ViewKey b 
    -> ViewKey c 
    -> ViewKey d 
    -> ViewKey e 
    -> ViewKey f 
    -> ViewKey g 
    -> View entity (a, b, c, d, e, f, g)

deriving instance Show (View entity keys)
deriving instance Eq (View entity keys)
instance Hashable (View entity keys) where
  hashWithSalt salt view = case view of
    ViewById -> 0
    ViewByKeys1 a -> hashWithSalt salt a
    ViewByKeys2 a b -> hashWithSalt salt (a, b)
    ViewByKeys3 a b c -> hashWithSalt salt (a, b, c)
    ViewByKeys4 a b c d -> hashWithSalt salt (a, b, c, d)
    ViewByKeys5 a b c d e -> hashWithSalt salt (a, b, c, d, e)
    ViewByKeys6 a b c d e f -> hashWithSalt salt (a, b, c, d, e, f)
    ViewByKeys7 a b c d e f g -> hashWithSalt salt (a, b, c, d, e, f, g)


viewGeneratedName :: View a k -> Maybe Text
viewGeneratedName view = case view of
  ViewById -> Nothing
  view -> Just $ pack . Base62.encodeSigned64 . fromIntegral . hash $ view

viewDocType :: (Entity a) => View a k -> Text
viewDocType = entityType . (undefined :: View a k -> a)

viewDesignName :: (Entity a) => View a k -> Maybe Text
viewDesignName ViewById = Nothing
viewDesignName view = entityType . (undefined :: View a k -> a) <$> Just view

viewKeysJS view = case view of
  ViewById -> Nothing
  ViewByKeys1 a -> Just $ toJS a
  ViewByKeys2 a b -> Just $ toJS (a, b)
  ViewByKeys3 a b c -> Just $ toJS (a, b, c)
  ViewByKeys4 a b c d -> Just $ toJS (a, b, c, d)
  ViewByKeys5 a b c d e -> Just $ toJS (a, b, c, d, e)
  ViewByKeys6 a b c d e f -> Just $ toJS (a, b, c, d, e, f)
  ViewByKeys7 a b c d e f g -> Just $ toJS (a, b, c, d, e, f, g)

viewMapFunctionJS :: (Entity a) => View a k -> Maybe Text
viewMapFunctionJS view = fmap concat $ sequence [
    pure "function (doc) { if (doc._id.lastIndexOf('",
    viewDesignName view,
    pure "-') == 0) emit(",
    viewKeysJS view,
    pure ", null) }"
  ]

viewPath :: (Entity a) => View a k -> [Text]
viewPath view = case view of
  ViewById -> ["_all_docs"]
  _ -> ["_design", fromMaybe undefined $ viewDesignName view, 
        "_view", fromMaybe undefined $ viewGeneratedName view]

createOrUpdateView :: (MonadAction m, Entity a) 
  => View a k 
  -> m (Persisted (DesignModel a))
createOrUpdateView view
  | Just name <- viewGeneratedName view,
    Just model <- ViewModel.View <$> viewMapFunctionJS view <*> pure Nothing
    = createOrUpdateDesignView name model
  | otherwise = error "EZCouch.View.createOrUpdateView: Attempt to persist a view which does not support it"
