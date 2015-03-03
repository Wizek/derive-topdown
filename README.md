# derive-topdown
This library will help you generate Haskell empty Generic instance and deriving type instances from the top automatically to the bottom.

An example to generate Out class for Person, Name and Address.
Out class in genericpretty package has to provide a default implementation for the function it declears.

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

================================

    Data\Derive\TopDown\Test.hs:1:1: Splicing declarations
        derivings ''Eq makeEq ''Person
      ======>
        Data\Derive\TopDown\Test.hs:23:1-30
        instance Eq Names where
          (==) (Names x1) (Names y1) = (x1 == y1)
        instance Eq Address where
          (==) (Address x1) (Address y1) = (x1 == y1)
        instance Eq Person where
          (==) (Person x1 x2) (Person y1 y2) = ((x1 == y1) && (x2 == y2))
          (==) (Student x1 x2) (Student y1 y2) = ((x1 == y1) && (x2 == y2))
          (==) _ _ = False
    Data\Derive\TopDown\Test.hs:1:1: Splicing declarations
        instances ''Out ''A
      ======>
        Data\Derive\TopDown\Test.hs:24:1-19
        instance Out a_a1IiG => Out (B a_a1IiG)
        instance (Out a_a1IiH, Out b_a1IiI) => Out (A a_a1IiH b_a1IiI)
    Data\Derive\TopDown\Test.hs:1:1: Splicing declarations
        instances ''Out ''Person
      ======>
        Data\Derive\TopDown\Test.hs:25:1-24
        instance Out Names
        instance Out Address
        instance Out Person

For generating 4 empty instances

    instance Out Person
    instnace Out Nmads
    instance Out Address
    instance Out Gate

you just write:

    instances ''Out ''Person

For derive Eq typeclass you just write

    derivings ''Eq makeEq ''Person

It will generate all instances that Person dependend on including Person.

Do not forget to use :set -ddump-splices, you will get

    instances ''Out  ''Person
    ======>
        ~\Test.hs:13:1-18
        instance Out Names
        instance Out Gate
        instance Out Address
        instance Out Person
        Ok, modules loaded: CompositeDataInstancesGen, Main.

You can also use instnaceList to generate a list of class. Solution 1 is to use derive package

    derivings ''Eq makeEq ''A 

If you enable -ddump-splices, you will get:

    Data\Derive\TopDown\Test.hs:1:1: Splicing declarations
       derives ''Eq makeEq ''A
     ======>
       Data\Derive\TopDown\Test.hs:18:1-25
       instance Eq a_1627720873 => Eq (B a_1627720873) where
         (==) (B x1) (B y1) = (x1 == y1)
       instance (Eq a_1627720874, Eq b_1627720875) =>
                Eq (A a_1627720874 b_1627720875) where
         (==) (A x1 x2) (A y1 y2) = ((x1 == y1) && (x2 == y2))

We have to specify makeEq or other Derivation values here and I am not sure how to eleminate it. 

Solution is is to use standalone deriving. it will be supposrt in GHC 7.10 standalone deriving will be supported and you do not need to write. Currently unimplemented. 

    deriving (Data, Typeable, Generic, Ord,Eq,Show)

for each data type declarations ever again. In stead you will be able to write:

    derivings instance [''Eq, ''Typeable, ''Generic] A
    
