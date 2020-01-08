module Utils.TTLCache where

import Prelude

import Data.Int (toNumber)
import Data.JSDate as JSDate
import Data.Map as M
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff, Milliseconds(Milliseconds), delay, forkAff, launchAff, launchAff_)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Ref as R
import Effect.Timer as T


type CacheItem a = { exp :: Number, lastUpdated :: Number, value :: a }
type Cache a = M.Map String (CacheItem a)

mkEmptyCache :: forall a. Effect (R.Ref (Cache a))
mkEmptyCache = R.new M.empty


withCachedItem_ :: ∀ v m. Bind m ⇒ MonadEffect m ⇒ R.Ref (Cache v) → String → (CacheItem v → m Unit) → m Unit
withCachedItem_ cache key f = do
  mItem <- liftEffect ((M.lookup key) <$> R.read cache )
  case mItem of 
    Nothing -> do 
      pure unit
    Just item' -> f item'

withCachedItem :: forall m v r. Bind m => MonadEffect m => R.Ref (Cache v) -> String -> (CacheItem v -> m (Maybe r)) -> m (Maybe r)
withCachedItem cache key f = do
  mItem <- liftEffect ((M.lookup key) <$> R.read cache )
  case mItem of 
    Nothing -> pure Nothing
    Just item' -> f item'

withCachedItemEarlyRenew :: forall m v r. Bind m => MonadEffect m => R.Ref (Cache v) -> Int -> Int -> String -> (v -> m r) -> m (Maybe r)
withCachedItemEarlyRenew cache ttl fresherThan key f = withCachedItem cache key (\ ({exp, value, lastUpdated}) -> do 
  now <- time
  -- liftEffect $ log (show now <> " <= " <> show lastUpdated <> " + " <> show fresherThan)
  if now <= lastUpdated + (toNumber fresherThan)
    then Just <$> f value
    else pure Nothing
)

time :: ∀ m. MonadEffect m ⇒ m Number
time = liftEffect $ JSDate.getTime <$> JSDate.now

wait :: ∀ u. Number → Aff u → Aff u
wait n f = delay (Milliseconds n) *> f

wait' :: Int → Effect Unit → Effect T.TimeoutId
wait' n = T.setTimeout n

checkExpiry :: ∀ val. R.Ref (M.Map String (CacheItem val)) → String → Effect Unit
checkExpiry cache key = 
  withCachedItem_ cache key (\ ({exp}) -> do
    now <- time
    let b = now - exp
    if b >= (toNumber 0)
      then void $ R.modify (M.delete key) cache
      else void $ launchAff $ forkAff $ wait b (liftEffect $ checkExpiry cache key)
  )

extendCache :: ∀ val. R.Ref (Cache val) → String → Int → Effect Unit
extendCache cache key ttl = do 
  withCachedItem_ cache key (\ ({exp, value, lastUpdated})  -> do
    void $ R.modify (M.insert key ( {exp: (exp + toNumber ttl), value, lastUpdated})) cache
    scheduleCleanup cache ttl key
  )

addCache :: ∀ val. R.Ref (Cache val) → Int → String → val → Effect Unit
addCache cache ttl key val = do
  now <- time
  let item = {exp: (now + (toNumber ttl)), value: val, lastUpdated: now}
  void $ R.modify (M.insert key item) cache
  scheduleCleanup cache ttl key


updateCache :: ∀ val. R.Ref (Cache val) → String → val → Effect Unit
updateCache cache key val = withCachedItem_ cache key (\ ({exp, lastUpdated}) -> do
  now <- time
  void $ R.modify (M.update (const $ Just $ {exp, value: val, lastUpdated: now}) key) cache
)

getCache :: ∀ val m. Bind m ⇒ MonadEffect m ⇒ R.Ref (M.Map String (CacheItem val)) → String → m (Maybe val)
getCache cache key = withCachedItem cache key (\ ({value})  -> pure $ Just value)

-- | Gets the cached value inside `cache` if `now <= exp - ttl`. Otherwise `Nothing`.
getCacheIfIsFresh :: ∀ val m. Bind m ⇒ MonadEffect m ⇒ R.Ref (Cache val) -> Int -> Int → String → m (Maybe val)
getCacheIfIsFresh cache ttl fresherThan key = withCachedItemEarlyRenew cache ttl fresherThan key pure


scheduleCleanup :: ∀ val. R.Ref (M.Map String (CacheItem val)) → Int → String → Effect Unit
scheduleCleanup cache ttl key =
  launchAff_ $ forkAff $ 
    void $ wait (toNumber $ ttl + 1000) $ do
      mItem <- liftEffect ((M.lookup key) <$> R.read cache )
      case mItem of 
        Nothing -> pure unit
        Just {exp, value} -> do
          now' <- liftEffect time
          let b = now' - exp
          if b >= (toNumber 0)
            then liftEffect $ void $ R.modify (M.delete key) cache
            else liftEffect $ pure unit



-- waitOnCache :: ∀ val e m. Monad m => MonadEffect e m => R.Ref (M.Map String (Tuple Number val)) → WaitOnCache m e val
-- waitOnCache :: ∀ t83 t94 t97 m. MonadEffect t83 m => R.Ref (M.Map String (Tuple t97 t94)) → String → Number → (Maybe t94 → m ( ref ∷ R.REF | t83 ) Boolean ) → m ( ref ∷ R.REF | t83 ) Unit
waitOnCache ::  forall val. R.Ref (M.Map String (CacheItem val)) -> String -> Number -> (Maybe val -> Effect Boolean) -> Effect Unit
waitOnCache cache key interval callback = 
  launchAff_ $ forkAff $ void $ wait interval $ do
    mItem <- liftEffect ((M.lookup key) <$> R.read cache )
    b <- liftEffect $ callback ((\ci -> ci.value) <$> mItem) 
    if b
      then pure unit
      else liftEffect $ waitOnCache cache key interval callback