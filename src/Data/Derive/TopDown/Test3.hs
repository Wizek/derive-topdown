{- OPTIONS_GHC -ddump-splices -}
import Data.Derive.TopDown.Derive
import Language.Haskell.TH

data D = D
data T = T

derives [makeEq, makeOrd] [''D, ''T]

-- :t $([| derive makeEq (mkName "D") |]) :: 
{--
foo :: Q [Dec]
foo = do
    q <- $(fmap head (derivings' ''Eq ''D))
    undefined --return $(head x) 
--}

-- $($(derivings'' ''Eq "D"))

foo = ($(derivings'' ''Eq "D"))
