{-# LANGUAGE OverloadedStrings, NoMonomorphismRestriction, FlexibleContexts, MultiParamTypeClasses, ScopedTypeVariables, DeriveDataTypeable, DeriveGeneric #-}
module EZCouch.Design where

import Prelude ()
import ClassyPrelude
import GHC.Generics
import EZCouch.Doc
import Data.Aeson
import qualified Network.HTTP.Types as HTTP
import qualified Network.HTTP.Conduit as HTTP

import EZCouch.ReadAction
import EZCouch.Action
import EZCouch.BulkOperationsAction
import EZCouch.Types
import EZCouch.Encoding
import EZCouch.Parsing


data Design a 
  = Design {
      views :: Map Text View
    }
  deriving (Show, Eq, Generic)
instance ToJSON (Design a)
instance FromJSON (Design a)
instance (Doc a) => Doc (Design a)

data View = View { map :: Text, reduce :: Maybe Text }
  deriving (Show, Eq, Generic)
instance ToJSON View
instance FromJSON View where
  parseJSON = withObject "View" $ \o -> 
    View <$> o .: "map" <*> o .:? "reduce"  

designName = docType . (undefined :: Design a -> a)

readDesign :: (MonadAction m, Doc a) => m (Maybe (Persisted (Design a)))
readDesign = result
  where
    result 
      = (flip catch) processException
        $ getAction ["_design", designName] [] "" 
          >>= parseSingleRow errorPersistedParser 
          >>= return . either (const Nothing) Just
      where
        designName = docType $ (undefined :: m (Maybe (Persisted (Design a))) -> a) result
        processException (HTTP.StatusCodeException (HTTP.Status 404 _) _) = return Nothing
        processException e = throwIO e

createOrUpdateDesign :: (MonadAction m, Doc a) => Design a -> m ()
createOrUpdateDesign design = do
  design' <- readDesign
  case design' of
    Just (Persisted id rev design'') -> if design'' == design
      then return ()
      else void $ update $ Persisted id rev design
    Nothing -> void $ createWithId id design
  where
    id = "_design/" ++ designName design

