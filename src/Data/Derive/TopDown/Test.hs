{-# LANGUAGE TemplateHaskell, DeriveGeneric,DeriveDataTypeable,TypeSynonymInstances #-}
{-# OPTIONS_GHC -ddump-splices #-}
module Data.Derive.TopDown.Test where 
import Data.Derive.TopDown.Generic
import Data.Derive.TopDown.Derive (derivings)
import Text.PrettyPrint.GenericPretty (Out)
import Data.DeriveTH
import GHC.Generics

data A a b = A a (B b) deriving (Show, Generic)
data B a = B a deriving (Show, Generic)

data Person = Person Names Address
            | Student Names Address deriving Generic
data Names  = Names String deriving Generic
data Address = Address Gate  deriving Generic
type Gate = (String,Int)

derivings ''Eq makeEq ''Person
instances ''Out ''A
instances ''Out ''Person





