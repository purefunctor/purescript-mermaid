module Mermaid.Do (bind, discard, module E) where

import Control.Alternative (pure) as E
import Control.Bind (class Bind)
import Prelude as Prelude

bind :: forall a b m. Bind m => m a -> (a -> m b) -> m b
bind = Prelude.bind

discard :: forall a b m. Bind m => m a -> (a -> m b) -> m b
discard = Prelude.bind
