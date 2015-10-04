
module Data.Derive.TopDown.Utils where
import Language.Haskell.TH
import Control.Monad (forM)
import Language.Haskell.TH.Utils

getCxtTyVarCons :: Name -> Q (Cxt, [TyVarBndr], [Con])
getCxtTyVarCons name = do
        info <- reify name
        case info of 
             TyConI dec ->
                case dec of
                     DataD    cxt _ tvbs cons _ -> return (cxt,tvbs,cons)
                     NewtypeD cxt _ tvbs con  _ -> return (cxt,tvbs,[con])
                     TySynD   name vars  type'  -> do
                                         let names = getTypeNames type'
                                         res <- forM names getCxtTyVarCons -- a :: [(Cxt, [TyVarBndr], [Con])]
                                         let (t1,t2,t3) = unzip3 res
                                         return (concat t1, concat t2 , concat t3)
                     -- You mat need TypeSynonymInstances to derive for type synonym
                     _ -> error "must be data, newtype definition or type synonym!"
             _ -> error "bad type name, quoted name is not a type!"



getCxtTyVarCons' :: Type ->  Q (Cxt, [TyVarBndr], [Con])
getCxtTyVarCons'(ConT t) = undefined --return ([],[],[NormalC])

applyContext :: Name -> [Name] -> Q [Pred]
applyContext con typeNames = return (map apply typeNames)
                         where apply t = ClassP con [VarT t]

