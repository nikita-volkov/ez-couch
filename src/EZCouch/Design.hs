{-# LANGUAGE OverloadedStrings, NoMonomorphismRestriction, FlexibleContexts, MultiParamTypeClasses, ScopedTypeVariables, DeriveDataTypeable, DeriveGeneric #-}
module EZCouch.Design where

import Prelude ()
import ClassyPrelude
import GHC.Generics
import EZCouch.Entity
import Data.Aeson
import qualified Network.HTTP.Types as HTTP
import qualified Network.HTTP.Conduit as HTTP
import qualified Data.Map as Map

import EZCouch.Action
import EZCouch.WriteAction
import EZCouch.Types
import EZCouch.Parsing

import EZCouch.Model.Design
import EZCouch.Model.View (View)
import qualified EZCouch.Model.View as View


readDesign :: (MonadAction m, Entity a) => m (Maybe (Persisted (Design a)))
readDesign = result
  where
    result = (flip catch) processException $ 
      getAction ["_design", designName] [] "" 
        >>= runParser errorPersistedParser
        >>= return . either (const Nothing) Just
      where
        designName = docType $ (undefined :: m (Maybe (Persisted (Design a))) -> a) result
        processException (HTTP.StatusCodeException (HTTP.Status 404 _) _) = return Nothing
        processException e = throwIO e

createOrUpdateDesign :: (MonadAction m, Entity a) => Design a -> m (Persisted (Design a))
createOrUpdateDesign design = 
  createDesign design `catch` \e -> case e of
    OperationException {} -> do
      design' <- readDesign
      case design' of
        Just design'@(Persisted id rev design'') -> if design'' == design
          then return design'
          else updateEntity $ Persisted id rev design
        Nothing -> throwIO e
    _ -> throwIO e

createDesign :: (MonadAction m, Entity a) => Design a -> m (Persisted (Design a))
createDesign design = createEntityWithId id design
  where
    id = "_design/" ++ designName design


updateDesignView :: (MonadAction m, Entity a)  
  => Persisted (Design a) -> Text -> View -> m (Persisted (Design a))
updateDesignView 
  design@(Persisted designId designRev (Design viewsMap))
  viewName
  view 
  | Just existingView <- lookup viewName viewsMap 
    = if existingView == view
        then return design
        else updateViewsMap $ Map.adjust (const $ view) viewName viewsMap
  | otherwise
    = updateViewsMap $ insert viewName view viewsMap
  where 
    updateViewsMap = updateEntity . Persisted designId designRev . Design

createOrUpdateDesignView :: (MonadAction m, Entity a)
  => Text -> View -> m (Persisted (Design a))
createOrUpdateDesignView viewName view = 
  createDesign newDesign `catch` \e -> case e of
    OperationException {} -> do
      existingDesign <- readDesign
      case existingDesign of
        Just existingDesign -> updateDesignView existingDesign viewName view
        Nothing -> throwIO e
    _ -> throwIO e
  where
    newDesign = Design $ fromList [(viewName, view)]
