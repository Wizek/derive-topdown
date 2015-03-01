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

        ~\Test.hs:13:1-18
        instance Out Names
        instance Out Gate
        instance Out Address
        instance Out Person
        Ok, modules loaded: CompositeDataInstancesGen, Main.

You can also use instnaceList to generate a list of class. 
Also in GHC 7.10 standalone deriving will be supported and you do not need to write

    deriving (Data, Typeable, Generic, Ord,Eq,Show)

for each data type declarations ever again.
