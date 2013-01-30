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
import EZCouch.WriteAction
import qualified EZCouch.Model.Design as DesignModel
import qualified EZCouch.Model.View as ViewModel


createOrUpdateViewDesign :: (Doc a, MonadAction m) => Text -> Maybe Text -> View a -> m (Persisted (DesignModel.Design a))
createOrUpdateViewDesign mapV reduceV view
  = readDesign >>= maybe create update'
  where
    create = createDesign $ DesignModel.Design $ fromList [(viewName view, viewModel)]
    update' design@(Persisted id rev (DesignModel.Design viewsMap))
      | Just viewModel' <- lookup viewName' viewsMap
        = if viewModel' == viewModel
            then return design
            else update $ Persisted id rev $ DesignModel.Design $ adjust (const $ viewModel) viewName' viewsMap
    viewName' = viewName view
    viewModel = ViewModel.View mapV reduceV

createOrUpdateView :: (Doc a, MonadAction m) 
  => View a     -- ^ view identifier
  -> Text       -- ^ /map/-function
  -> Maybe Text -- ^ /reduce/-function
  -> m ()
createOrUpdateView view map reduce = void $ createOrUpdateViewDesign map reduce view
