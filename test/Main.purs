module Test.Main where

import Prelude

import Control.Apply ((*>))
import Control.Coroutine
import Control.Coroutine.Aff
import Control.Monad
import Control.Monad.Aff
import Control.Monad.Eff
import Control.Monad.Eff.Class
import Control.Monad.Eff.Console
import Control.Monad.Eff.Random
import Control.Monad.Eff.Ref
import Control.Monad.Rec.Class
import Control.Monad.Free.Trans
import Data.Tuple
import Control.Monad.ST
import Data.Array hiding (filter)
import Data.Array.ST
import Data.Either
import Data.Foldable
import Data.Functor (($>))
import Data.Generic
import Data.Maybe
import Data.Tuple

import Test.Unit
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen
import Test.Process

import Debug.Trace

import Data.CouchDB
import Control.Coroutine.CouchDB

popSTArray :: forall a h eff. STArray h a -> Eff (st :: ST h | eff) (Maybe a)
popSTArray stArr = do
  arr <- freeze stArr
  spliceSTArray stArr 0 1 []
  return $ head arr

getStubGet :: forall a b eff1 eff2.
  Array (Either Unit a) ->
  Eff (st :: ST b | eff1) (Int -> Eff (st :: ST b | eff2) (Either Unit a))
getStubGet results = do
  mutableResults <- thaw results
  return $ \_ -> do
    currentResult <- popSTArray mutableResults
    return $ maybe (Left unit) id currentResult

collect' :: forall a m r. (Monad m) => Transformer a (Array a) m r
collect' = tailRecM go []
  where
  go :: Array a -> Transformer a (Array a) m (Either (Array a) r)
  go xs = liftFreeT $ Transform \x -> Tuple (xs <> [x]) (Left (xs <> [x]))

type AsyncQuickCheckEff eff =
  Eff (exit :: EXIT, console :: CONSOLE, ref :: REF, random :: RANDOM | eff) Unit

type AsyncQuickCheckable a eff =
  (Boolean -> AsyncQuickCheckEff eff) -> a -> AsyncQuickCheckEff eff

asyncQuickCheck :: forall a eff. (Arbitrary a) =>
  String -> Int -> AsyncQuickCheckable a eff -> AsyncQuickCheckEff eff
asyncQuickCheck s n f = do
  refTestResults <- newRef []
  let done result = do
        modifyRef refTestResults (result :)
        testResults <- readRef refTestResults
        if (length testResults == n) then allDone testResults else return unit
  liftEff $ randomSample' n arbitrary >>= (flip foreachE) (f done)
    where
    allDone xs = case ((foldl (&&) true xs)) of
      true -> log $ "  ✔︎ " ++ s
      false -> log ("  ✘ " ++ s) *> exit 1

take' :: forall a m. (Monad m) => Int -> Transformer (Array a) (Maybe (Array a)) m Unit
take' n = forever (transform maybeTake)
  where
  maybeTake :: forall a. Array a -> Maybe (Array a)
  maybeTake xs | length xs == n = Just $ take n xs
  maybeTake _ = Nothing

transform' :: forall a b m. (Monad m) => (a -> b) -> Transformer (Maybe a) (Maybe b) m Unit
transform' g = forever (transform (>>= (return <<< g)))

main = do
  log "produceNotifications:" *> do
    asyncQuickCheck "Should produce notifications sequentially." 50 \done notifications -> do
      let nonEmptyNotifications = fst notifications : snd notifications
      let results = (Right <$> nonEmptyNotifications) :: Array (Either Unit Notification)
      stubGet <- liftEff $ getStubGet results
      let p = produceNotifications (liftEff <<< stubGet) 0
      let eqNonEmptyNotifications = transform' (gEq nonEmptyNotifications)
      let t = collect' ~~ (take' (length nonEmptyNotifications)) ~~ eqNonEmptyNotifications
      let c = consumer (($> Nothing) <<< liftEff <<< maybe (return unit) done)
      launchAff $ runProcess (p $~ t $$ c)
