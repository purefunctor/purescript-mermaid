module Mermaid.Hyrule where

import Prelude

import Control.Monad.ST (ST)
import Control.Monad.ST.Global (Global)
import Data.Profunctor (dimap)
import Effect (Effect)
import FRP.Event (AnEvent)
import Mermaid (Mermaid, liftImpure, liftPure, runImpure, runPure)
import Unsafe.Coerce (unsafeCoerce)

type MdEvent r = AnEvent (Mermaid r)

fromStEvent :: forall r. AnEvent (ST r) ~> MdEvent r
fromStEvent = unsafeUnwrapEvent >>> dimap asSt asMd >>> unsafeWrapEvent
  where
  asSt :: forall a. (a -> Mermaid r Unit) -> (a -> ST r Unit)
  asSt = map runPure

  asMd :: ST r (ST r Unit) -> Mermaid r (Mermaid r Unit)
  asMd = map liftPure <<< liftPure

  unsafeUnwrapEvent
    :: forall a. AnEvent (ST r) a -> ((a -> ST r Unit) -> ST r (ST r Unit))
  unsafeUnwrapEvent = unsafeCoerce

  unsafeWrapEvent
    :: forall a. ((a -> Mermaid r Unit) -> Mermaid r (Mermaid r Unit)) -> MdEvent r a
  unsafeWrapEvent = unsafeCoerce

toStEvent :: forall r. MdEvent r ~> AnEvent (ST r)
toStEvent = unsafeUnwrapEvent >>> dimap asMd asSt >>> unsafeWrapEvent
  where
  asMd :: forall a. (a -> ST r Unit) -> (a -> Mermaid r Unit)
  asMd = map liftPure

  asSt :: Mermaid r (Mermaid r Unit) -> ST r (ST r Unit)
  asSt = map runPure <<< runPure

  unsafeUnwrapEvent
    :: forall a. MdEvent r a -> ((a -> Mermaid r Unit) -> Mermaid r (Mermaid r Unit))
  unsafeUnwrapEvent = unsafeCoerce

  unsafeWrapEvent
    :: forall a. ((a -> ST r Unit) -> ST r (ST r Unit)) -> AnEvent (ST r) a
  unsafeWrapEvent = unsafeCoerce

fromEfEvent :: AnEvent Effect ~> MdEvent Global
fromEfEvent = unsafeUnwrapEvent >>> dimap asEf asMd >>> unsafeWrapEvent
  where
  asEf :: forall a. (a -> Mermaid Global Unit) -> (a -> Effect Unit)
  asEf = map runImpure

  asMd :: Effect (Effect Unit) -> Mermaid Global (Mermaid Global Unit)
  asMd = map liftImpure <<< liftImpure

  unsafeUnwrapEvent
    :: forall a. AnEvent Effect a -> ((a -> Effect Unit) -> Effect (Effect Unit))
  unsafeUnwrapEvent = unsafeCoerce

  unsafeWrapEvent
    :: forall a
     . ((a -> Mermaid Global Unit) -> Mermaid Global (Mermaid Global Unit))
    -> MdEvent Global a
  unsafeWrapEvent = unsafeCoerce

toEfEvent :: MdEvent Global ~> AnEvent Effect
toEfEvent = unsafeUnwrapEvent >>> dimap asMd asEf >>> unsafeWrapEvent
  where
  asMd :: forall a. (a -> Effect Unit) -> (a -> Mermaid Global Unit)
  asMd = map liftImpure

  asEf :: Mermaid Global (Mermaid Global Unit) -> Effect (Effect Unit)
  asEf = map runImpure <<< runImpure

  unsafeUnwrapEvent
    :: forall a
     . MdEvent Global a
    -> ((a -> Mermaid Global Unit) -> Mermaid Global (Mermaid Global Unit))
  unsafeUnwrapEvent = unsafeCoerce

  unsafeWrapEvent
    :: forall a
     . ((a -> Effect Unit) -> Effect (Effect Unit))
    -> AnEvent Effect a
  unsafeWrapEvent = unsafeCoerce
