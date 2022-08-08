module Mermaid.Bolson where

import Prelude

import Bolson.Core
  ( Child(..)
  , DynamicChildren(..)
  , Entity(..)
  , EventfulElement(..)
  , FixedChildren(..)
  )
import Control.Monad.ST (ST)
import Control.Monad.ST.Global (Global)
import Effect (Effect)
import Mermaid (Mermaid)
import Mermaid.Hyrule (fromEfEvent, fromStEvent)

fromStDynamicChildren
  :: forall logic obj r lock
   . DynamicChildren logic obj (ST r) lock
  -> DynamicChildren logic obj (Mermaid r) lock
fromStDynamicChildren (DynamicChildren dynamicChildren) =
  DynamicChildren $ (map fromStChild <<< fromStEvent) <$> fromStEvent dynamicChildren

fromStFixedChildren
  :: forall logic obj r lock
   . FixedChildren logic obj (ST r) lock
  -> FixedChildren logic obj (Mermaid r) lock
fromStFixedChildren (FixedChildren fixedChildren) =
  FixedChildren $ fromStEntity <$> fixedChildren

fromStEventfulElement
  :: forall logic obj r lock
   . EventfulElement logic obj (ST r) lock
  -> EventfulElement logic obj (Mermaid r) lock
fromStEventfulElement (EventfulElement eventfulElement) =
  EventfulElement $ fromStEntity <$> fromStEvent eventfulElement

fromStChild
  :: forall logic obj r lock. Child logic obj (ST r) lock -> Child logic obj (Mermaid r) lock
fromStChild = case _ of
  Insert entity -> Insert $ fromStEntity entity
  Remove -> Remove
  Logic logic -> Logic logic

fromStEntity
  :: forall logic obj r lock. Entity logic obj (ST r) lock -> Entity logic obj (Mermaid r) lock
fromStEntity = case _ of
  DynamicChildren' dynamicChildren -> DynamicChildren' $ fromStDynamicChildren dynamicChildren
  FixedChildren' fixedChildren -> FixedChildren' $ fromStFixedChildren fixedChildren
  EventfulElement' eventfulElement -> EventfulElement' $ fromStEventfulElement eventfulElement
  Element' object -> Element' object

fromEfDynamicChildren
  :: forall logic obj lock
   . DynamicChildren logic obj Effect lock
  -> DynamicChildren logic obj (Mermaid Global) lock
fromEfDynamicChildren (DynamicChildren dynamicChildren) =
  DynamicChildren $ (map fromEfChild <<< fromEfEvent) <$> fromEfEvent dynamicChildren

fromEfFixedChildren
  :: forall logic obj lock
   . FixedChildren logic obj Effect lock
  -> FixedChildren logic obj (Mermaid Global) lock
fromEfFixedChildren (FixedChildren fixedChildren) =
  FixedChildren $ fromEfEntity <$> fixedChildren

fromEfEventfulElement
  :: forall logic obj lock
   . EventfulElement logic obj Effect lock
  -> EventfulElement logic obj (Mermaid Global) lock
fromEfEventfulElement (EventfulElement eventfulElement) =
  EventfulElement $ fromEfEntity <$> fromEfEvent eventfulElement

fromEfChild
  :: forall logic obj lock. Child logic obj Effect lock -> Child logic obj (Mermaid Global) lock
fromEfChild = case _ of
  Insert entity -> Insert $ fromEfEntity entity
  Remove -> Remove
  Logic logic -> Logic logic

fromEfEntity
  :: forall logic obj lock. Entity logic obj Effect lock -> Entity logic obj (Mermaid Global) lock
fromEfEntity = case _ of
  DynamicChildren' dynamicChildren -> DynamicChildren' $ fromEfDynamicChildren dynamicChildren
  FixedChildren' fixedChildren -> FixedChildren' $ fromEfFixedChildren fixedChildren
  EventfulElement' eventfulElement -> EventfulElement' $ fromEfEventfulElement eventfulElement
  Element' object -> Element' object
