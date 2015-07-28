module Data.Dani 
    ( 
        module Data.Dani.Types
    ,   value
    ,   value_
    ) where 

import Data.Dani.Types

import Control.Comonad.Trans.Cofree
import Control.Comonad.Env

value :: a 
      -> [Cofree ValueF b] 
      -> ValueF (CofreeT ValueF (Env [Cofree ValueF b]) a)
      -> CofreeT ValueF (Env [Cofree ValueF b]) a
value a e = CofreeT . env e . (:<) a

value_ :: a -> ValueF (Cofree ValueF a) -> Cofree ValueF a
value_ a = cofree . (:<) a

-- askMeta :: CofreeT ValueF (Env [Cofree ValueF b]) a
--         -> [Cofree ValueF b]
-- askMeta = ask . lower



