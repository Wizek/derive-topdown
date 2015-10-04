{-# LANGUAGE TemplateHaskell  #-}

module StandaloneDerive (deriveTopdown,derivings,generic_instances, instances ) where

import Language.Haskell.TH
import Control.Monad.State
import Control.Monad.Trans (lift)
import Debug.Trace
import Data.List (foldl')
import qualified GHC.Generics as G

deriveTopdown :: Name -- ^ class name
              -> Name -- ^ type name
              -> Bool -- ^ with Generic context
              -> Q [Dec]
deriveTopdown cn tn g = evalStateT (gen cn tn g) []

derivings ::Bool -> [Name] -- ^ class names
               -> Name   -- ^ type name
               -> Q [Dec]
derivings g cnms t = fmap concat (sequence $ map (\x -> deriveTopdown x t g) cnms)

instances = False
generic_instances = True

gen :: Name -> Name -> Bool -> StateT [Type] Q [Dec]
gen cn tn withGeneric = do
       (tvbs,cons) <- lift $ getTyVarCons tn
       let typeNames = map getTVBName  tvbs
       -- isinstanceType = (A a b) will be used as context
       instanceType <- lift $ foldl' appT (conT tn) $ map varT typeNames --  A a b
       --- Eq a, Eq b .....
       let derive_context = (map (AppT (ConT cn)) (map VarT typeNames)) ++ 
                                 if withGeneric then map (AppT (ConT ''G.Generic)) (map VarT typeNames)
                                 else []
       --- (Eq a , Eq b , ...)
       let derive_context_in_tuple = foldl1 AppT $ (TupleT (length derive_context)) : derive_context

       isMember <- if tvbs == []
                then lift $ isInstance cn [instanceType]
                else lift $ isInstance cn [ForallT tvbs [] instanceType] --not working
       table <- get

       if isMember || elem instanceType table
          then return []
       -- (Eq a, Eq b) => Eq (A a b)
       -- standalone driving: deriving instance (Eq a , Eq b) => Eq (A a b) 
          else do
            let c = [StandaloneDerivD [derive_context_in_tuple] (AppT (ConT cn) instanceType)]
            modify (instanceType:)
            let names = concatMap getCompositeType cons
            xs <-  mapM (\n -> gen cn n withGeneric) names
            return $ concat xs ++ c

getCompositeType :: Con -> [Name]
getCompositeType (NormalC n sts)        = concatMap getTypeNames (map snd sts)
getCompositeType (RecC    n vars)       = concatMap getTypeNames (map third vars)
getCompositeType (InfixC st1 n st2)     = concatMap getTypeNames [snd st1 , snd st2]
-- This could be a problem since it will lose info for context and type variables 
getCompositeType (ForallC tvbs cxt con) = getCompositeType con

getTypeNames :: Type -> [Name]
getTypeNames (ForallT tvbs cxt t) = getTypeNames t
getTypeNames (ConT n) = [n]
getTypeNames (AppT t1 t2) = getTypeNames t1 ++ getTypeNames t2
getTypeNames _ = []

third (a,b,c) = c

applyConT :: [Type] -> Type
applyConT = foldr1 AppT

getTVBName :: TyVarBndr -> Name
getTVBName (PlainTV  name  ) = name
getTVBName (KindedTV name _) = name

getTyVarCons :: Name -> Q ([TyVarBndr], [Con])
getTyVarCons name = do
        info <- reify name
        case info of 
             TyConI dec ->
                case dec of
                     DataD    _ _ tvbs cons _ -> return (tvbs,cons)
                     NewtypeD _ _ tvbs con  _ -> return (tvbs,[con])
                     TySynD   _ vars type'    -> undefined -- need to handle type eta reduction
                     _ -> error "must be data, newtype definition or type synonym!"
             _ -> error "bad type name, quoted name is not a type!"
