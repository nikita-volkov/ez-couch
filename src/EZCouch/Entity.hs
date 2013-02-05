{-# LANGUAGE OverloadedStrings, NoMonomorphismRestriction, FlexibleContexts, MultiParamTypeClasses, ScopedTypeVariables, DeriveDataTypeable, FlexibleInstances, TypeSynonymInstances, DefaultSignatures, OverlappingInstances, TypeOperators, DeriveGeneric #-}
module EZCouch.Entity where

import Prelude ()
import ClassyPrelude 

import GHC.Generics
import Data.Aeson

class (ToJSON a, FromJSON a) => Entity a where
  entityType :: a -> Text

  default entityType :: (Generic a, GEntity (Rep a)) => a -> Text
  entityType = gEntityType . from

class GEntity f where 
  gEntityType :: f a -> Text

instance (GEntity a) => GEntity (M1 i c a) where
  gEntityType = gEntityType . unM1

instance (Constructor c) => GEntity (C1 c a) where
  gEntityType = const . pack $ conName (undefined :: t c a p)

instance (GEntity a, GEntity b) => GEntity (a :+: b) where
  gEntityType (L1 x) = gEntityType x
  gEntityType (R1 x) = gEntityType x
