module Mermaid.Do (bind, discard, module E) where

import Control.Alternative (pure) as E
import Control.Bind (class Bind)
import Data.Unit (Unit)
import Prelude as Prelude

bind :: forall a m. Bind m => m a -> (a -> m Unit) -> m Unit
bind = Prelude.bind

discard :: forall a m. Bind m => m a -> (a -> m Unit) -> m Unit
discard = bind
