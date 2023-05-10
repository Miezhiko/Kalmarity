module Kalmarity.Common
  ( module Exported
  , (.:)
  ) where

import           Prelude.Unicode as Exported

(.:) ∷ (b -> c) -> (a1 -> a2 -> b) -> a1 -> a2 -> c
(.:) = (∘) ∘ (∘)
