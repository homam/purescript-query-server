module Server.Query where

import Data.Array (head)
import Data.Bifunctor (lmap)
import Data.Either (Either(..), either)
import Data.JSDate as JSDate
import Data.Maybe (Maybe(..), maybe)
import Database.Postgres as PG
import Effect (Effect)
import Effect.Aff (Aff, Canceler(..), Error, Fiber, attempt, cancelWith, error, forkAff, joinFiber, killFiber, launchAff, throwError)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Effect.Exception as X
import Effect.Ref (Ref)
import Foreign (Foreign)
import Prelude (class Show, bind, discard, identity, mempty, not, pure, show, void, ($), (&&), (<$>), (<<<), (<>), (>>=), (||))
import Simple.JSON as JSON
import Utils.TTLCache as C

type QueryCache = C.Cache QueryState

mkQueryCache :: C.Cache QueryState
mkQueryCache = mempty


data QueryState = Running ( Fiber (Either X.Error (Array Foreign))) JSDate.JSDate | Error String | Cancelled | Done (Array Foreign)

isDone :: QueryState → Boolean
isDone (Done _) = true
isDone _ = false

isRunning :: QueryState → Boolean
isRunning (Running _ _) = true
isRunning _ = false

instance showQueryState ::  Show QueryState where
  show (Running a b) = "Running"
  show (Error e) = "Error " <> e
  show Cancelled = "Cancelled"
  show (Done as) = "Done"

read' :: forall a. JSON.ReadForeign a => Foreign -> Either Error a
read' = lmap (error <<< show) <<< JSON.read

type RunDbQuery b c =  
    String
  -> Aff c
    
type RunDbQueryAsync = 
     Boolean
  -> String
  -> String 
  → Aff QueryState 

type RunDbQuerySync = 
      Boolean 
   -> String 
   -> String 
   -> Aff (Array Foreign)

type KillQuery  = 
    String 
  → Effect (Either String String)

type GetQueryState = 
    String 
  → Effect (Maybe QueryState )

queryAsync :: 
    Ref (C.Cache QueryState)
  → {queryPool :: PG.Pool, adminPool :: PG.Pool}
  → RunDbQueryAsync 
queryAsync cache {queryPool, adminPool} nocache qid q = do
  mcached <- C.getCache cache qid
  case mcached of 
    Just cached -> 
      if not nocache && (isDone cached || isRunning cached)
        then pure cached
        else go
    Nothing -> go

  where 
  go = do
    fiber <- PG.withClient queryPool $ \client -> do 
      recs <- PG.query_ (read':: Foreign -> Either Error ( {clientid :: Int})) (PG.Query ("SELECT pg_backend_pid() as clientid" )) client
      let queryId = maybe 0 identity ((\c -> c.clientid) <$> head recs)
      liftEffect $ log (show $ (\c -> c.clientid) <$> head recs)
      forkAff $ do 
        cancelWith (do 
            results <- attempt $ PG.query_ read' (PG.Query q :: PG.Query Foreign) client
            liftEffect $ log $ "Done " <> qid
            liftEffect $ C.updateCache cache qid  (either (Error <<< show) Done results)
            pure results
        ) $ Canceler (\_ -> do 
          liftEffect $ log "cacnelling!"
          adminResult <- attempt $ PG.withClient adminPool $ \adminClient -> do
            liftEffect $ log "cancelling connected..."
            PG.query_ (read' :: Foreign -> Either Error ({cancelled :: Int})) (PG.Query ("select pg_cancel_backend(" <> (show queryId) <> ") as cancelled;")) adminClient
          liftEffect $ log (show adminResult)
          liftEffect $ log "cancelled!"
        )

    now <- liftEffect JSDate.now
    let st = Running fiber now
    liftEffect $ C.addCache cache 600000 qid st
    pure st


getQueryState :: Ref (C.Cache QueryState) → String → Effect (Maybe QueryState)
getQueryState cache qid = C.getCache cache qid 

killQuery :: Ref (C.Cache QueryState) → KillQuery 
killQuery cache qid = do
  mqs <-  getQueryState cache qid
  case mqs of
    Nothing -> pure $ Left $ "No query was found for " <> qid
    Just qs -> case qs of
      Running fiber _ -> do
        void $ launchAff $ killFiber (error "User Cancelled") fiber
        C.updateCache cache qid Cancelled
        pure $ Right "Killed"
      x -> pure $ Left $ "Invalid Query State: " <> show x


querySync :: Ref (C.Cache QueryState) → {queryPool :: PG.Pool, adminPool :: PG.Pool} → RunDbQuerySync
querySync cache pool nocache hash sqlTemplate = do
  qs <- queryAsync cache pool nocache hash sqlTemplate
  case qs of
    Running fiber _ -> (joinFiber fiber) >>=
      either
        (throwError <<< error <<< show)
        pure
    Done result -> pure result
    Error e -> throwError $ error (show e)
    Cancelled -> throwError (error "Cancelled")