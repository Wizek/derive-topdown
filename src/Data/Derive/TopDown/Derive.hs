{-# LANGUAGE TemplateHaskell , QuasiQuotes, RankNTypes #-}

module Data.Derive.TopDown.Derive (
derivings
) where

import Data.Derive.TopDown.Utils
import Language.Haskell.TH
import Language.Haskell.TH.Utils

import Control.Monad (forM)
import Data.List (foldl')
import Control.Monad.State
import Control.Monad.Trans (lift)
import Debug.Trace
import qualified Language.Haskell.TH.Syntax as S
import Data.DeriveTH

-- | deriving from top
{-|
> data A a b = A a (B b) deriving (Show)
> data B a = B a deriving (Show)
>
> derivings ''Eq makeEq ''A 
If you enable -ddump-splices, you will get:
>
> Data\Derive\TopDown\Test.hs:1:1: Splicing declarations
>    derives ''Eq makeEq ''A
>  ======>
>    Data\Derive\TopDown\Test.hs:18:1-25
>    instance Eq a_1627720873 => Eq (B a_1627720873) where
>      (==) (B x1) (B y1) = (x1 == y1)
>    instance (Eq a_1627720874, Eq b_1627720875) =>
>             Eq (A a_1627720874 b_1627720875) where
>      (==) (A x1 x2) (A y1 y2) = ((x1 == y1) && (x2 == y2))

This will make sense if you have a deep composited data types, nomally an AST of a language.
For now, you have to specify both of ''Eq and makeEq, I suppose ''Eq will be enough.
To look what typeclasses you can derive, see 'derive' library on hackage.
-}
derivings :: Name -> Derivation -> Name -> Q [Dec]
derivings className dv typeName  = (fmap fst ((runStateT $ gen className typeName dv) []))

-- derivings :: Name -> Name -> Q [Dec]

-- class name , type name
gen :: Name -> Name -> Derivation -> StateT [Type] Q [Dec]
gen cla tp  dv = do
    (cxt,tvbs,cons) <- lift $ getCxtTyVarCons tp
    let typeNames = map getTVBName tvbs
    instanceType <- lift $ foldl' appT (conT tp) $ map varT typeNames
    context      <- lift $ applyContext cla typeNames
    isMember <- if tvbs == []
                       then lift $ isInstance cla [instanceType]
                       else lift $ isInstance cla [ForallT tvbs cxt instanceType]
    table <- get
    if isMember || elem instanceType table
       then return []
       else do
            let makeClassName = mkName $ "make" ++ nameBase cla
            let tpname = nameBase tp
            dec <- lift (derive dv tp)
            modify (instanceType:)
            let names = concatMap getCompositeType cons
            xs <-  mapM (\n -> gen cla n dv) names
            return $ concat xs ++ dec        

---- Please ignore the following
---- I am trying to implement without specifying makeEq, or makeOrd ...
derivings' :: Name -> Name -> Q [Exp]
derivings' className typeName  = (fmap fst ((runStateT $ gen' className typeName) []))

gen' :: Name -> Name ->  StateT [Type] Q [Exp]
gen' cla tp = do
    (cxt,tvbs,cons) <- lift $ getCxtTyVarCons tp
    let typeNames = map getTVBName tvbs
    instanceType <- lift $ foldl' appT (conT tp) $ map varT typeNames
    context      <- lift $ applyContext cla typeNames
    isMember <- if tvbs == []
                       then lift $ isInstance cla [instanceType]
                       else lift $ isInstance cla [ForallT tvbs cxt instanceType]
    table <- get
    if isMember || elem instanceType table
       then return []
       else do
            let makeClassName = mkName $ "make" ++ nameBase cla
            let tpname = nameBase tp
            dec <- lift $ appExp [(varE (mkName "derive")), (varE makeClassName), (varE tp)]
            -- ****** how to splice [Exp] to [Dec] ?!
            lift [| derive $(varE makeClassName) tp |]
            modify (instanceType:)
            let names = concatMap getCompositeType cons
            xs <-  mapM (\n -> gen' cla n ) names
            return $ concat xs ++ [dec]        

-- data D = D

derivings'' :: Name -> Name -> Q Exp
derivings'' cla typ = do
               let makeClassName = mkName $ "make" ++ nameBase cla
               
               a <-  [| derive makeClassName (typ) |]
               return a


instance S.Lift Name where
         lift x = varE x

{--
isI = do
    t <-  [t| forall a. Eq a => [a] |]
    t1 <- [t| Int |]
    t2 <- [t| forall a. [a] |]
    isInstance ''Eq  [t]
-}
existentialTypeContainsClass :: Name -> Type -> Q Bool
existentialTypeContainsClass clss (ForallT _ cxt t) = return $ or $ map (boundByPred clss) cxt

boundByPred :: Name -> Pred -> Bool
boundByPred _ (EqualP _ _)    = False
boundByPred c (ClassP clss _) = c == clss

t = [t| forall a b . (Eq a)=> (a,b) |]
t' = do 
   t1 <- t
   return $ ForallT [PlainTV (mkName "a")] [ClassP ''Eq [VarT (mkName "a")]] t1

runTest = do 
        t1 <- t
        existentialTypeContainsClass ''Eq t1
