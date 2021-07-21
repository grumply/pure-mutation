{-# LANGUAGE CPP, PatternSynonyms, MultiParamTypeClasses, RecordWildCards,
   TypeFamilies, DeriveGeneric, DeriveAnyClass, OverloadedStrings, 
   FlexibleContexts, ViewPatterns, TemplateHaskell #-}
module Pure.Mutation where

import Pure hiding (attributes,features,children,Action,action,root)

import Pure.Data.Lifted
import Pure.Data.JSON
import Pure.Data.Prop.TH

import Control.Arrow ((&&&))
import Control.Monad
import Data.Coerce
import Data.Foldable (for_,traverse_)
import Data.Function ((&))
import Data.IORef
import Data.Maybe
import GHC.Generics as G

#ifdef __GHCJS__
import GHCJS.Marshal.Internal
import JavaScript.Object.Internal as JS (Object(..),create,setProp)
#endif

data Mutation 
  = AttributeMutation
    { target :: Node
    , attributeName :: Txt
    , attributeNamespace :: Txt
    , oldAttribute :: Maybe Txt
    , newAttribute :: Maybe Txt
    }
  | ContentMutation
    { target :: Node
    , oldContent :: Maybe Txt
    , newContent :: Maybe Txt
    }
  | ChildrenMutation
    { target :: Node
    , addedNodes :: [Node]
    , removedNodes :: [Node]
    , previousSibling :: Maybe Node
    , nextSibling :: Maybe Node
    }

data Observer = Observer_
  { as :: Features -> [View] -> View
  , features :: Features
  , children :: [View]
  , subtree :: Bool
  , childList :: Bool
  , attributes :: Bool
  , attributeFilter :: [Txt]
  , attributeOldValue :: Bool
  , characterData :: Bool
  , characterDataOldValue :: Bool
  , action :: [Mutation] -> IO () 
  , disable :: Bool
  } deriving Generic

deriveLocalComponent ''Observer

instance Default Observer where
  def = (G.to gdef) 
    { as = \fs cs -> Div & Features fs & Children cs }

toOptions :: Observer -> IO JSV
toOptions o = do
#ifdef __GHCJS__
  obj <- JS.create
  when (subtree o) (JS.setProp "subtree" (pToJSVal True) obj)
  when (childList o) (JS.setProp "childList" (pToJSVal True) obj)
  when (attributes o) (JS.setProp "attributes" (pToJSVal True) obj)
  mas <- if null (attributeFilter o) 
         then pure Nothing 
         else Just <$> toJSValListOf (attributeFilter o)
  for_ mas $ \as -> JS.setProp "attributeFilter" as obj 
  when (attributeOldValue o) (JS.setProp "attributeOldValue" (pToJSVal True) obj)
  when (characterData o) (JS.setProp "characterData" (pToJSVal True) obj)
  when (characterDataOldValue o) (JS.setProp "characterDataOldValue" (pToJSVal True) obj)
  pure (coerce obj)
#else
  pure ()
#endif

instance Pure Observer where
  view =
    Component $ \self ->
      let
        withNode node = modifyM_ self $ \o _ -> return ((Nothing,node),run o)
        run o = do
          unless (disable o) $ do
            (_,node) <- get self
            options <- toOptions o
            (obs,rel) <- observer (action o)
            observe obs node options
            modify_ self $ \_ _ -> (Just rel,node)
      in
        def
          { construct = pure (Nothing,coerce nullJSV)
          , receive = \new (mrel,node) -> traverse_ id mrel >> run new >> return (mrel,node)
          , unmounted = get self >>= \(mrel,_) -> traverse_ id mrel
          , render = \Observer_ {..} (_,node) -> as (features & Host node withNode) children
          }

mkMutation :: JSV -> IO Mutation
mkMutation jsv =
#ifdef __GHCJS__
    let
      Just s = jsv .# "type"
      Just t = jsv .# "target"
    in case s of
        "attributes"    -> mkAttributeMutation t jsv
        "characterData" -> mkContentMutation t jsv
        "childList"     -> mkChildrenMutation t jsv
        _               -> error ("Pure.Mutation.mkMutation: unknown mutation event type " ++ fromTxt s)
  where
    mkAttributeMutation :: JSV -> JSV -> IO Mutation
    mkAttributeMutation t jsv = do
      let 
        target = coerce t
        attributeName = fromMaybe "" (jsv .# "attributeName")
        attributeNamespace = fromMaybe "" (jsv .# "attributeNamespace")
        oldAttribute = jsv .# "oldValue"
        newAttribute = t .# attributeName
      pure AttributeMutation {..} 

    mkContentMutation :: JSV -> JSV -> IO Mutation
    mkContentMutation t jsv = do
      let 
        target = coerce t
        oldContent = jsv .# "oldValue"
        newContent = t .# "textContent"
      pure ContentMutation {..}

    mkChildrenMutation :: JSV -> JSV -> IO Mutation
    mkChildrenMutation t jsv = do
      let 
        target = coerce t
        previousSibling = (coerce :: Maybe JSV -> Maybe Node) (jsv .# "previousSibling")
        nextSibling = (coerce :: Maybe JSV -> Maybe Node) (jsv .# "nextSibling")
        Just added = jsv .# "addedNodes"
        Just removed = jsv .# "removedNodes"
      (maybe [] (coerce :: [JSV] -> [Node]) -> addedNodes) <- fromJSValListOf added
      (maybe [] (coerce :: [JSV] -> [Node]) -> removedNodes) <- fromJSValListOf removed
      pure ChildrenMutation {..}
#else
  pure (AttributeMutation (coerce ()) def def def def)
#endif

#ifdef __GHCJS__
foreign import javascript unsafe
  "$r = new MutationObserver($1)" observer_js :: Callback (JSV -> IO ()) -> IO JSV

foreign import javascript unsafe
  "$1.observe($2,$3)" observe_js :: JSV -> Node -> JSV -> IO ()

foreign import javascript unsafe
  "$1.disconnect()" disconnect_js :: JSV -> IO ()

foreign import javascript unsafe
  "$r = $1.parentNode" parent_js :: JSV -> IO JSV
#endif

observer :: ([Mutation] -> IO ()) -> IO (JSV,IO ())
observer f = do
#ifdef __GHCJS__
  cb <- syncCallback1 ContinueAsync $ \arr -> do
    Just jsvs <- fromJSValListOf arr 
    ms <- traverse mkMutation jsvs
    f ms
  obs <- observer_js cb
  pure (obs,releaseCallback cb)
#else
  pure (def,pure ())
#endif

observe :: JSV -> Node -> JSV -> IO ()
observe obsrvr node opts =
#ifdef __GHCJS__
  observe_js obsrvr node opts
#else
  pure ()
#endif

disconnect :: JSV -> IO ()
disconnect obsrvr =
#ifdef __GHCJS__
  disconnect_js obsrvr
#else
  pure ()
#endif

parent :: JSV -> IO JSV
parent =
#ifdef __GHCJS__
  parent_js
#else
  pure
#endif