{-# LANGUAGE OverloadedStrings #-}

module Data.Dani.Encode (
        module Data.Dani
    ) where


import Data.Dani
import Data.Text.Lazy.Builder.Scientific
import qualified Data.Text.Lazy.Builder as T

encodeToTextBuilder :: Value -> T.Builder
encodeToTextBuilder = undefined

encodeToTextBuilder_ :: Value_ -> T.Builder
encodeToTextBuilder_ = undefined
