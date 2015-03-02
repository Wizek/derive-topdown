# derive-topdown
This library will help you generate Haskell empty Generic instance and deriving type instances from the top automatically to the bottom.

{-|
An example to generate Out class for Person, Name and Address.
Out class in genericpretty package has to provide a default implementation for the function it declears.

    data Person = Person Names Address 
                | Student Names Address 
                  deriving (Show, Generic, Eq, Ord , Data,Typeable)
    data Names  = Names String 
                  deriving (Show, Generic, Eq, Ord, Data, Typeable)
    data Address = Address Gate
                  deriving (Show, Generic, Eq, Ord, Typeable, Data)

    type Gate = PF

    data PF = PF String deriving (Data, Typeable, Generic, Ord,Eq,Show)


For generating 4 empty instances

    instance Out Person
    instnace Out Nmads
    instance Out Address
    instance Out Gate

 you just write:

    instances ''Out ''Person

It will generate all instances that form Person and including Person.

If you use :set -ddump-splices, you will get

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
    
