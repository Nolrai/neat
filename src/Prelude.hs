-- | Uses [relude](https://hackage.haskell.org/package/relude) as default Prelude.
module Prelude
  ( module Relude,
    sshow,
  )
where

import Relude

sshow :: (Semigroup s, Show a, IsString s) => s -> a -> s
s `sshow` a = s <> show a
