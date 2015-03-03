{-# LANGUAGE TemplateHaskell  #-}
module Data.Derive.TopDown.Generic ((-->),instances, instanceList) where

import Data.List (foldl')
import Control.Monad.State
import Control.Monad.Trans (lift)
import Language.Haskell.TH
import Data.Derive.TopDown.Utils
import Language.Haskell.TH.Utils
{-|
An example to generate Out class for Person, Name and Address.
Out class has to provide a default implementation for the function it declears.

@
data Person = Person Names Address 
            | Student Names Address 
              deriving (Show, Generic, Eq, Ord , Data,Typeable)
data Names  = Names String 
              deriving (Show, Generic, Eq, Ord, Data, Typeable)
data Address = Address Gate
              deriving (Show, Generic, Eq, Ord, Typeable, Data)

type Gate = PF

data PF = PF String deriving (Data, Typeable, Generic, Ord,Eq,Show)
@
For generating 4 empty instances
> instance Out Person
> instnace Out Nmads
> instance Out Address
> instance Out Gate

you just write:
@
instances ''Out ''Person
@
It will generate all instances that form Person and including Person.

If you use :set -ddump-splices, you will get
>  instances ''Out  ''Person
>  ======>
>  ~\Test.hs:13:1-18
>    instance Out Names
>    instance Out Gate
>    instance Out Address
>    instance Out Person
> Ok, modules loaded: CompositeDataInstancesGen, Main.
You can also use instnaceList to generate a list of class. 
-}

-- | synatx sugar
instances = deriveInstances
(-->)     = deriveInstances

-- | Generate instances for a list of classes with default implementation
instanceList :: Name -> [Name] -> Q [Dec]
instanceList cla ls = fmap concat $ mapM (instances cla) ls

-- | Generate a single instance for a typeclass
deriveInstances :: Name -> Name -> Q [Dec]
deriveInstances className typeName = (fmap fst ((runStateT $ gen className typeName) []))

-- gen class name, type name 
gen :: Name -> Name -> StateT [Type] Q [Dec]
gen cla tp = do 
    (cxt,tvbs,cons) <- lift $ getCxtTyVarCons tp
    let typeNames = map getTVBName tvbs
    instanceType <- lift $ foldl' appT (conT tp) $ map varT typeNames
    context      <- lift $ applyContext cla typeNames
    let declTypes = conT cla `appT` (return instanceType)
    isMember <- if tvbs == []
                       then lift $ isInstance cla [instanceType]
                            ---- Actually the following line will not word
                       else lift $ isInstance cla [ForallT tvbs cxt instanceType]
    table <- get
    if isMember || elem instanceType table
       then return []
       else do
            dec <- lift $ fmap (:[]) $ instanceD (return context) declTypes []
            modify (instanceType:)
            let names = concatMap getCompositeType cons
            xs <-  mapM (\n -> gen cla n) names
            return $ concat xs ++ dec


