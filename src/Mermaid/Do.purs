module Mermaid.Do (bind, discard, module E) where

import Control.Alt (void)
import Control.Alternative (pure) as E
import Control.Bind (class Bind)
import Data.Unit (Unit)
import Prelude as Prelude

bind :: forall a b m. Bind m => m a -> (a -> m b) -> m Unit
bind m f = void (Prelude.bind m f)

discard :: forall a b m. Bind m => m a -> (a -> m b) -> m Unit
discard m f = void (bind m f)
