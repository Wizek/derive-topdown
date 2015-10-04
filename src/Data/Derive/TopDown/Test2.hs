{-# LANGUAGE TemplateHaskell  #-}

{-# OPTIONS_GHC -ddump-splices #-}
--import Data.Derive.TopDown.Test
import Data.Derive.TopDown.Derive
import Language.Haskell.TH.Utils
import Language.Haskell.TH

data D = D

foo = do
    a <- $(derivings'' ''Eq ''D)
    return a
