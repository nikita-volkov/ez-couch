{-# LANGUAGE OverloadedStrings, NoMonomorphismRestriction, FlexibleContexts, MultiParamTypeClasses, ScopedTypeVariables, DeriveDataTypeable, DeriveGeneric, GADTs, StandaloneDeriving #-}
module EZCouch.View where

import Prelude ()
import ClassyPrelude
import GHC.Generics
import Data.Aeson
import Data.Generics (Data, Typeable)
import EZCouch.Action
import EZCouch.Doc
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
  hash = hash . toJS

-- TODO: rename ViewById and ViewByKeyN
data View entity keys where
  ViewAll 
    :: View entity Text
  ViewKeys1 
    :: ViewKey a 
    -> View entity a
  ViewKeys2 
    :: ViewKey a 
    -> ViewKey b 
    -> View entity (a, b)
  ViewKeys3 
    :: ViewKey a 
    -> ViewKey b 
    -> ViewKey c 
    -> View entity (a, b, c)
  ViewKeys4 
    :: ViewKey a 
    -> ViewKey b 
    -> ViewKey c 
    -> ViewKey d 
    -> View entity (a, b, c, d)
  ViewKeys5 
    :: ViewKey a 
    -> ViewKey b 
    -> ViewKey c 
    -> ViewKey d 
    -> ViewKey e 
    -> View entity (a, b, c, d, e)
  ViewKeys6 
    :: ViewKey a 
    -> ViewKey b 
    -> ViewKey c 
    -> ViewKey d 
    -> ViewKey e 
    -> ViewKey f 
    -> View entity (a, b, c, d, e, f)
  ViewKeys7 
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
  hash view = case view of
    ViewAll -> 0
    ViewKeys1 a -> hash a
    ViewKeys2 a b -> hash (a, b)
    ViewKeys3 a b c -> hash (a, b, c)
    ViewKeys4 a b c d -> hash (a, b, c, d)
    ViewKeys5 a b c d e -> hash (a, b, c, d, e)
    ViewKeys6 a b c d e f -> hash (a, b, c, d, e, f)
    ViewKeys7 a b c d e f g -> hash (a, b, c, d, e, f, g)


viewGeneratedName :: View a k -> Maybe Text
viewGeneratedName view = case view of
  ViewAll -> Nothing
  view -> Just $ pack . Base62.encodeSigned64 . fromIntegral . hash $ view

viewDocType :: (Doc a) => View a k -> Text
viewDocType = docType . (undefined :: View a k -> a)

viewDesignName :: (Doc a) => View a k -> Maybe Text
viewDesignName ViewAll = Nothing
viewDesignName view = docType . (undefined :: View a k -> a) <$> Just view

viewKeysJS view = case view of
  ViewAll -> Nothing
  ViewKeys1 a -> Just $ toJS a
  ViewKeys2 a b -> Just $ toJS (a, b)
  ViewKeys3 a b c -> Just $ toJS (a, b, c)
  ViewKeys4 a b c d -> Just $ toJS (a, b, c, d)
  ViewKeys5 a b c d e -> Just $ toJS (a, b, c, d, e)
  ViewKeys6 a b c d e f -> Just $ toJS (a, b, c, d, e, f)
  ViewKeys7 a b c d e f g -> Just $ toJS (a, b, c, d, e, f, g)

viewMapFunctionJS :: (Doc a) => View a k -> Maybe Text
viewMapFunctionJS view = fmap concat $ sequence [
    pure "function (doc) { if (doc._id.lastIndexOf('",
    viewDesignName view,
    pure "-') == 0) emit(",
    viewKeysJS view,
    pure ", null) }"
  ]

viewPath :: (Doc a) => View a k -> [Text]
viewPath view = case view of
  ViewAll -> ["_all_docs"]
  _ -> ["_design", fromMaybe undefined $ viewDesignName view, 
        "_view", fromMaybe undefined $ viewGeneratedName view]

createOrUpdateView :: (MonadAction m, Doc a) 
  => View a k 
  -> m (Persisted (DesignModel a))
createOrUpdateView view
  | Just name <- viewGeneratedName view,
    Just model <- ViewModel.View <$> viewMapFunctionJS view <*> pure Nothing
    = createOrUpdateDesignView name model
  | otherwise = error "EZCouch.View.createOrUpdateView: Attempt to persist a view which does not support it"
