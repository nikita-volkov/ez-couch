{-# LANGUAGE OverloadedStrings, NoMonomorphismRestriction, FlexibleContexts, MultiParamTypeClasses, ScopedTypeVariables, DeriveDataTypeable, DeriveGeneric #-}
module EZCouch.View where

import Prelude ()
import ClassyPrelude
import Data.Aeson
import Data.Map (adjust)
import EZCouch.Action
import EZCouch.Doc
import EZCouch.Types
import EZCouch.Design 
import EZCouch.BulkOperationsAction
import qualified EZCouch.Model.Design as DesignModel
import qualified EZCouch.Model.View as ViewModel


createOrUpdateView :: (Doc a, MonadAction m) => Text -> Maybe Text -> View a -> m ()
createOrUpdateView mapV reduceV view
  = readViewDesign view >>= maybe create update'
  where
    create = void $ createDesign $ newViewDesign mapV reduceV view
    update' design@(Persisted id rev (DesignModel.Design viewsMap))
      | Just viewModel' <- lookup viewName' viewsMap
        = if viewModel' == viewModel
            then return ()
            else void $ update $ asTypeOf design $ Persisted id rev $ DesignModel.Design $ adjust (const $ viewModel) viewName' viewsMap
    viewName' = viewName view
    viewModel = ViewModel.View mapV reduceV

readViewDesign :: (Doc a, MonadAction m) => View a -> m (Maybe (Persisted (DesignModel.Design a)))
readViewDesign = const readDesign

newViewDesign :: (Doc a) => Text -> Maybe Text -> View a -> DesignModel.Design a
newViewDesign mapV reduceV view = DesignModel.Design $ fromList [(viewName view, ViewModel.View mapV reduceV)]