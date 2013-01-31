{-# LANGUAGE OverloadedStrings, NoMonomorphismRestriction, FlexibleContexts, MultiParamTypeClasses, ScopedTypeVariables, DeriveDataTypeable, DeriveGeneric, GADTs, StandaloneDeriving #-}
module EZCouch.View where

import Prelude ()
import ClassyPrelude
import GHC.Generics
import Data.Aeson
import Data.Map (adjust)
import Data.Generics (Data, Typeable)
import EZCouch.Action
import EZCouch.Doc
import EZCouch.Types
import EZCouch.Design 
import EZCouch.WriteAction
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


data View entity keys where
  -- | No specific view
  ViewAll 
    :: View entity Text
  -- | A view which selects the entities by a single `ViewKey`
  ViewKeys1 
    :: ViewKey a 
    -> View entity a
  -- | A view which selects the entities by a pair of `ViewKey`s
  ViewKeys2 
    :: ViewKey a 
    -> ViewKey b 
    -> View entity (a, b)
  -- | A view which selects the entities by a triple of `ViewKey`s
  ViewKeys3 
    :: ViewKey a 
    -> ViewKey b 
    -> ViewKey c 
    -> View entity (a, b, c)
  -- | ...
  ViewKeys4 
    :: ViewKey a 
    -> ViewKey b 
    -> ViewKey c 
    -> ViewKey d 
    -> View entity (a, b, c, d)
  -- | ...
  ViewKeys5 
    :: ViewKey a 
    -> ViewKey b 
    -> ViewKey c 
    -> ViewKey d 
    -> ViewKey e 
    -> View entity (a, b, c, d, e)
  -- | ...
  ViewKeys6 
    :: ViewKey a 
    -> ViewKey b 
    -> ViewKey c 
    -> ViewKey d 
    -> ViewKey e 
    -> ViewKey f 
    -> View entity (a, b, c, d, e, f)
  -- | ...
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

-- data A = A deriving (Generic)
-- instance ToJSON A
-- instance FromJSON A
-- instance Doc A
-- main = do
--   Foldable.mapM_ putStrLn $ viewMapFunctionJS $ view
--   Foldable.mapM_ putStrLn $ viewGeneratedName $ view
--   where
--     view :: View A (Text, Text)
--     view = ViewKeys2 (ViewKeyRandom) (ViewKeyField "ab.sd.d")
      

-- queryView :: View entity keys -> QueryViewOptions -> Value




-- createOrUpdateViewDesign :: (Doc a, MonadAction m) => Text -> Maybe Text -> View a -> m (Persisted (DesignModel.Design a))
-- createOrUpdateViewDesign mapV reduceV view
--   = readDesign >>= maybe create update'
--   where
--     create = createDesign $ DesignModel.Design $ fromList [(viewGeneratedName view, viewModel)]
--     update' design@(Persisted id rev (DesignModel.Design viewsMap))
--       | Just viewModel' <- lookup viewGeneratedName' viewsMap
--         = if viewModel' == viewModel
--             then return design
--             else update $ Persisted id rev $ DesignModel.Design $ adjust (const $ viewModel) viewGeneratedName' viewsMap
--     viewGeneratedName' = viewGeneratedName view
--     viewModel = ViewModel.View mapV reduceV

-- createOrUpdateView :: (Doc a, MonadAction m) 
--   => View a     -- ^ view identifier
--   -> Text       -- ^ /map/-function
--   -> Maybe Text -- ^ /reduce/-function
--   -> m ()
-- createOrUpdateView view map reduce = void $ createOrUpdateViewDesign map reduce view



-- createOrUpdateView :: (MonadAction m) => View a k -> m ()

