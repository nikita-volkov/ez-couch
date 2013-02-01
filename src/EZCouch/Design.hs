{-# LANGUAGE OverloadedStrings, NoMonomorphismRestriction, FlexibleContexts, MultiParamTypeClasses, ScopedTypeVariables, DeriveDataTypeable, DeriveGeneric #-}
module EZCouch.Design where

import Prelude ()
import ClassyPrelude
import GHC.Generics
import EZCouch.Doc
import Data.Aeson
import qualified Network.HTTP.Types as HTTP
import qualified Network.HTTP.Conduit as HTTP

import EZCouch.Action
import EZCouch.WriteAction
import EZCouch.Types
import EZCouch.Parsing

import EZCouch.Model.Design


readDesign :: (MonadAction m, Doc a) => m (Maybe (Persisted (Design a)))
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

createOrUpdateDesign :: (MonadAction m, Doc a) => Design a -> m ()
createOrUpdateDesign design = 
  (void $ createDesign design) `catch` \e -> case e of
    OperationException {} -> do
      design' <- readDesign
      case design' of
        Just (Persisted id rev design'') -> if design'' == design
          then return ()
          else void $ update $ Persisted id rev design
        Nothing -> throwIO e
    _ -> throwIO e

createDesign :: (MonadAction m, Doc a) => Design a -> m (Persisted (Design a))
createDesign design = createWithId id design
  where
    id = "_design/" ++ designName design
