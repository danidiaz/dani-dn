module Data.Dani 
    ( 
        module Data.Dani.Types
    ,   value
    ,   value_
    ) where 

import Data.Dani.Types

import Control.Comonad.Trans.Cofree
import Control.Comonad.Env


--type Value = CofreeT ValueF (Env [Value_]) ()

value_ :: a -> ValueF (Cofree ValueF a) -> Cofree ValueF a
value_ a = cofree . (:<) a

value :: a 
      -> [Cofree ValueF b] 
      -> ValueF (CofreeT ValueF (Env [Cofree ValueF b]) a)
      -> CofreeT ValueF (Env [Cofree ValueF b]) a
value a e = CofreeT . env e . (:<) a
