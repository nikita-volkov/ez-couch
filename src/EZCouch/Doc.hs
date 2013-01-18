{-# LANGUAGE OverloadedStrings, NoMonomorphismRestriction, FlexibleContexts, MultiParamTypeClasses, ScopedTypeVariables, DeriveDataTypeable, FlexibleInstances, TypeSynonymInstances, DefaultSignatures, OverlappingInstances, TypeOperators, DeriveGeneric #-}
module EZCouch.Doc where

import Prelude ()
import ClassyPrelude 

import GHC.Generics
import Data.Aeson

class (ToJSON a, FromJSON a) => Doc a where
  docType :: a -> ByteString

  default docType :: (Generic a, GDoc (Rep a)) => a -> ByteString
  docType = gDocType . from

class GDoc f where 
  gDocType :: f a -> ByteString

instance (GDoc a) => GDoc (M1 i c a) where
  gDocType = gDocType . unM1

instance (Constructor c) => GDoc (C1 c a) where
  gDocType = const . encodeUtf8 . pack $ conName (undefined :: t c a p)

instance (GDoc a, GDoc b) => GDoc (a :+: b) where
  gDocType (L1 x) = gDocType x
  gDocType (R1 x) = gDocType x
