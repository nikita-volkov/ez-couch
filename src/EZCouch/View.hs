{-# LANGUAGE OverloadedStrings, NoMonomorphismRestriction, FlexibleContexts, MultiParamTypeClasses, ScopedTypeVariables, DeriveDataTypeable, DeriveGeneric, GADTs, StandaloneDeriving, QuasiQuotes #-}
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
import NeatInterpolation


type ViewModel = ViewModel.View
type DesignModel = DesignModel.Design

data Path = 
  PathField Text Path |
  PathItem Path |
  PathNil
  deriving (Show, Eq)

pathJS :: Path -> Text -> Text
pathJS (PathNil) js = js
pathJS (PathField name tail) js =
  pathJS tail $ 
    [text|
      $js
        .map( function( it ){ return it.$name } )
    |]
pathJS (PathItem tail) js =
  pathJS tail $ 
    [text|
      join( 
        $js
      )
    |]

data ViewKey a = 
  ViewKeyValue Path |
  -- ^ A path to a field value.
  ViewKeyRandom
  -- ^ This will emit a JavaScript @Math.random()@ value as a key. This is what 
  -- makes the querying for random entities possible.
  deriving (Show, Eq)

instance ToJS (ViewKey a) where
  toJS (ViewKeyValue path) = pathJS path "[ doc ]"
  toJS ViewKeyRandom = "[ Math.random() ]"

instance Hashable (ViewKey a) where
  hashWithSalt salt = hashWithSalt salt . toJS

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
  view -> Just $ pack . Base62.fromSigned64 . fromIntegral . hash $ view

viewDocType :: (Entity a) => View a k -> Text
viewDocType = entityType . (undefined :: View a k -> a)

viewDesignName :: (Entity a) => View a k -> Maybe Text
viewDesignName ViewById = Nothing
viewDesignName view = entityType . (undefined :: View a k -> a) <$> Just view

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

viewKeysJS :: Entity a => View a k -> Maybe Text
viewKeysJS view = case view of
  ViewById -> Nothing
  ViewByKeys1 a -> Just $ toJS a
  ViewByKeys2 a b -> Just $ 
    let 
      aJS = toJS a
      bJS = toJS b
    in 
      [text| 
        combinations([ 
          $aJS, 
          $bJS 
        ])
      |]
  ViewByKeys3 a b c -> Just $ 
    let 
      aJS = toJS a
      bJS = toJS b
      cJS = toJS c
    in 
      [text| 
        combinations([ 
          $aJS, 
          $bJS, 
          $cJS 
        ])
      |]
  ViewByKeys4 a b c d -> Just $ 
    let 
      aJS = toJS a
      bJS = toJS b
      cJS = toJS c
      dJS = toJS d
    in 
      [text| 
        combinations([ 
          $aJS, 
          $bJS, 
          $cJS, 
          $dJS 
        ])
      |]
  ViewByKeys5 a b c d e -> Just $ 
    let 
      aJS = toJS a
      bJS = toJS b
      cJS = toJS c
      dJS = toJS d
      eJS = toJS e
    in 
      [text| 
        combinations([ 
          $aJS, 
          $bJS, 
          $cJS, 
          $dJS, 
          $eJS 
        ])
      |]
  ViewByKeys6 a b c d e f -> Just $ 
    let 
      aJS = toJS a
      bJS = toJS b
      cJS = toJS c
      dJS = toJS d
      eJS = toJS e
      fJS = toJS f
    in 
      [text| 
        combinations([ 
          $aJS, 
          $bJS, 
          $cJS, 
          $dJS, 
          $eJS, 
          $fJS 
        ])
      |]
  ViewByKeys7 a b c d e f g -> Just $ 
    let 
      aJS = toJS a
      bJS = toJS b
      cJS = toJS c
      dJS = toJS d
      eJS = toJS e
      fJS = toJS f
      gJS = toJS g
    in 
      [text| 
        combinations([ 
          $aJS, 
          $bJS, 
          $cJS, 
          $dJS, 
          $eJS, 
          $fJS, 
          $gJS 
        ])
      |]


viewMapFunctionJS :: (Entity a) => View a k -> Maybe Text
viewMapFunctionJS view = 
  mapFunctionJS <$> viewDesignName view <*> viewKeysJS view

mapFunctionJS :: Text -> Text -> Text
mapFunctionJS designName expr = 
  [text|
    function( doc ){
      function startsWith( start, string ){
        return string.lastIndexOf( start ) == 0
      }
      function join( it ){ 
        return [].concat.apply( [], it ) 
      }
      function tail( array ){ 
        return array.slice(1)
      }
      function cons( head, array ){ 
        return [ head ].concat(array) 
      }
      function combinations( arrays ){
        if( arrays.length == 0 ) return []
        else if( arrays.length == 1 ) return arrays[0]
        else return join( 
          arrays[0].map( function( it ){ 
            return combinations( tail( arrays ) ).map( function( row ){
              return cons( it, row )
            } )
          } )
        )
      }
      function zip( arrays ){
        return arrays[0].map( function( _, i ){
          return arrays.map( function( array ){ return array[i] } )
        } )
      }

      if( startsWith( '$designName-', doc._id ) ){
        $expr
          .forEach( function( row ){ 
            emit( row, null ) 
          } )
      }
    }
  |]
