{-# LANGUAGE OverloadedStrings, NoMonomorphismRestriction, FlexibleContexts, MultiParamTypeClasses, ScopedTypeVariables, DeriveDataTypeable, DeriveGeneric #-}
module EZCouch.JS where

import Prelude ()
import ClassyPrelude
import GHC.Generics
import Data.Aeson
import qualified Data.Text.Lazy as LText

class ToJS a where
  toJS :: a -> Text

instance (ToJS a, ToJS b) => 
  ToJS (a, b) 
  where
    toJS (a, b) = "[ " 
      ++ toJS a ++ ", " 
      ++ toJS b ++ " ]"

instance (ToJS a, ToJS b, ToJS c) => 
  ToJS (a, b, c) 
  where
    toJS (a, b, c) = "[ " 
      ++ toJS a ++ ", " 
      ++ toJS b ++ ", " 
      ++ toJS c ++ " ]"

instance (ToJS a, ToJS b, ToJS c, ToJS d) => 
  ToJS (a, b, c, d) 
  where
    toJS (a, b, c, d) = "[ " 
      ++ toJS a ++ ", " 
      ++ toJS b ++ ", " 
      ++ toJS c ++ ", " 
      ++ toJS d ++ " ]"

instance (ToJS a, ToJS b, ToJS c, ToJS d, ToJS e) => 
  ToJS (a, b, c, d, e) 
  where
    toJS (a, b, c, d, e) = "[ " 
      ++ toJS a ++ ", " 
      ++ toJS b ++ ", " 
      ++ toJS c ++ ", " 
      ++ toJS d ++ ", " 
      ++ toJS e ++ " ]"

instance (ToJS a, ToJS b, ToJS c, ToJS d, ToJS e, ToJS f) => 
  ToJS (a, b, c, d, e, f) 
  where
    toJS (a, b, c, d, e, f) = "[ " 
      ++ toJS a ++ ", " 
      ++ toJS b ++ ", " 
      ++ toJS c ++ ", " 
      ++ toJS d ++ ", " 
      ++ toJS e ++ ", " 
      ++ toJS f ++ " ]"

instance (ToJS a, ToJS b, ToJS c, ToJS d, ToJS e, ToJS f, ToJS g) => 
  ToJS (a, b, c, d, e, f, g) 
  where
    toJS (a, b, c, d, e, f, g) = "[ " 
      ++ toJS a ++ ", " 
      ++ toJS b ++ ", " 
      ++ toJS c ++ ", " 
      ++ toJS d ++ ", " 
      ++ toJS e ++ ", " 
      ++ toJS f ++ ", "
      ++ toJS g ++ " ]"


newtype JSON a = JSON a

instance (ToJSON a) => ToJS (JSON a) where
  toJS (JSON a) = LText.toStrict . decodeUtf8 . encode $ a
