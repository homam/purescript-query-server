module Server.Server where

import Prelude hiding (apply)

import Control.Monad.Error.Class (throwError)
import Data.Either (Either(..), either)
import Data.Function.Uncurried (Fn1, Fn3, runFn1, runFn3)
import Data.Int (fromString, toNumber)
import Data.List (List(..))
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Number as Number
import Data.Tuple (Tuple(..))
import Database.Postgres as PG
import Effect (Effect)
import Effect.Aff (Error, attempt)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Effect.Console as Console
import Effect.Exception (error, throw)
import Effect.Ref as Ref
import Node.Express.App (App, get, listenHttp)
import Node.Express.Handler (HandlerM(..), Handler)
import Node.Express.Request (getRouteParam)
import Node.Express.Response (send, setStatus)
import Node.Express.Types (Response)
import Node.HTTP (Server)
import Node.Process (lookupEnv)
import QueryStringPSQL.Context (LMapType(..), QueryContext, QueryEngine(..), TimezoneInfo(..))
import QueryStringPSQL.Parser.BreakdownParser (runBreakdownParser)
import QueryStringPSQL.Parser.FiltersParser (runFiltersParser)
import QueryStringPSQL.ToSql (breakdownToSqlGroupBy, timestampFiltersToSqlWhere, toSql)
import Server.Query (GetQueryState, KillQuery, QueryCache, RunDbQueryAsync, RunDbQuerySync, getQueryState, killQuery, mkQueryCache, queryAsync, querySync)

newtype AppState = AppState {
    queryCache :: Ref.Ref QueryCache
  , queryAsync :: RunDbQueryAsync 
  , querySync :: RunDbQuerySync
  , getQueryState :: GetQueryState  
  , killQuery :: KillQuery 
}

foreign import _sendFile :: Fn3 Response String (Error -> Effect Unit) (Effect Unit)
sendFile :: String -> (Error -> Effect Unit) -> Handler
sendFile path callback = HandlerM \_ resp _ ->
    liftEffect $ runFn3 _sendFile resp path callback

foreign import _pathResolve :: Fn1 String String
pathResolve :: String -> String
pathResolve p = runFn1 _pathResolve p


requiredRouteParam :: forall r. String -> (String -> Maybe r) -> String -> HandlerM r
requiredRouteParam name f err = requiredRouteParam' name $ maybe (Left err) (Right <<< identity) <<< f

requiredRouteParam'' :: String -> HandlerM String
requiredRouteParam'' name = requiredRouteParam name pure ""

requiredRouteParam' :: forall r e. Show e => String -> (String -> Either e r) -> HandlerM r
requiredRouteParam' name f = 
      getRouteParam name 
  >>= maybe (throwError (error $ name <> " parameter in path is mandatory" )) pure 
  >>= either (throwError <<< error <<< show) pure <<< f

app :: App
app = do 
    -- cache <- liftEffect $ Ref.new mkQueryCache
    connStr <- liftEffect $ lookupEnv "jewel_connection_string" >>= \ m -> case m of
      Just a -> pure a
      Nothing -> throw "Expected jewel_connection_string ENV variable."
    (AppState state) <- liftEffect $ connect connStr  
    -- just for testing kill function  
    get "/api/test/kill/:qid" $ do 
      qid <- requiredRouteParam'' "qid"
      r <- liftEffect $ state.killQuery qid
      send r
    get "/api/test/get/:qid" $ do 
      qid <- requiredRouteParam'' "qid"
      r <- liftEffect $ state.getQueryState qid
      send r
    get "/api/test/run/:fromTimestamp/:qid" $ do 
      qid <- requiredRouteParam'' "qid"
      fromTimestampRouteParam <- requiredRouteParam'' "fromTimestamp"
      r <- liftAff $ attempt $ state.queryAsync false qid ("select * from user_sessions where timestamp > '" <> fromTimestampRouteParam <> "' order by user_agent limit 100000")
      send {"waiting": "..."}
    -- ^^ just for testing kill function
    get "/api/:timezone/:fromTimestamp/:toTimestamp/:filters/:breakdowns" $ do 
        timezoneRouteParam <- requiredRouteParam "timezone" Number.fromString "timezone must be a number"
        fromTimestampRouteParam <- requiredRouteParam'' "fromTimestamp"
        toTimestampRouteParam <- requiredRouteParam'' "toTimestamp"
        filters <- requiredRouteParam' "filters" runFiltersParser
        breakdowns <- requiredRouteParam' "breakdowns" runBreakdownParser

        -- query <- getQueryParam "query" >>= maybe (throwError (error "query param eter is mandatory")) pure
        -- ttl <- (getQueryParam "ttl" >>= maybe (pure $ 10 * 60 * 1000) pure )
        -- redir <- (getQueryParam "redir" >>= maybe (pure false) (\s -> pure $ s == "true" || (readFloat s > toNumber 0)) )
        -- fresherThan <- (getQueryParam "ft" >>= maybe (pure $ 5 * 60 * 1000) (pure <<< (\x -> x * 1000) <<< fromMaybe 300 <<< fromNumber <<< readFloat) )

        let params = { 
          timezone : timezoneRouteParam,
          dateFrom : fromTimestampRouteParam,
          dateTo : toTimestampRouteParam,
          breakdown : Nil,
          filters : filters
        } -- :: QueryParams

        -- let filters = toSql params contextUserSessionsRedshift <$> runFiltersParser filterRouteParam
        let timestampWhere = timestampFiltersToSqlWhere contextUserSessionsRedshift params

        let query = """
          SELECT """ <> toSql params contextUserSessionsRedshift breakdowns <> """,
              sum(case when us.impression > 0 or us.redirect > 0 then 1 else 0 end) :: float as views
            , sum(case when us.sale > 0 then 1 else 0 end) :: float as sales
            , sum(case when us.failedsale > 0 then 1 else 0 end) :: float as failedsales
            , sum(case when us.pixel > 0 or us.delayed_pixel > 0 then 1 else 0 end) :: float as pixels
            , sum(case when us.firstbilling > 0 then 1 else 0 end) :: float as firstbillings
            , sum(coalesce(ub.home_cpa, 0)) :: float as cost
            , sum(case when us.optout > 0 then 1 else 0 end) :: float as optouts
            , sum((us.viewport_width > 0 AND us.has_focus and us.is_visible) :: integer) :: float as premium_sessions
            , sum((us.viewport_width > 0 AND us.has_focus and us.is_visible and us.sale > 0) :: integer) :: float as premium_sales
            , sum(case when us.optout > 0 and date_diff('hours', us.sale_timestamp, us.optout_timestamp) < 24 then 1 else 0 end) :: float as optout_24h
            , sum(case when us.resubscribe > 0 then 1 else 0 end) :: float as resubs
            , sum(case when us.mouseclick > 0 or us.touch > 0 then 1 else 0 end) :: float as clicks_or_touches
            , sum(case when us.lead1 > 0 then 1 else 0 end) :: float as lead1s
            , sum(case when us.lead2 > 0 then 1 else 0 end) :: float as lead2s
            , sum(case when us.lead1 > 0 or us.lead2 > 0 then 1 else 0 end) :: float as any_leads
            , sum(case when us.resubscribe > 0 and (us.pixel > 0 or us.delayed_pixel > 0) then 1 else 0 end) :: float as pixels_for_resubs
            , sum(case when us.firstbilling <= 0 and (us.pixel > 0 or us.delayed_pixel > 0) then 1 else 0 end) :: float as pixels_for_no_firstbilling
            , sum(case when 
                      (
                            coalesce(us.sale, 0) > 0 
                        and coalesce(us.firstbilling, 0) > 0 
                        and coalesce(us.resubscribe, 0) <= 0 
                      )
                        and NOT (us.pixel > 0 or us.delayed_pixel > 0) 
                      then 1 else 0 end
                ) :: float as missed_good_pixels
            , sum(case when us.get_sub_method ilike '%block%' then 1 else 0 end) :: integer as blocks
            , sum(case when json_extract_path_text(us.query_string,'utm_cdn') <> '' then 1 else 0 end) :: integer as safe
          FROM user_sessions as us
          LEFT JOIN user_subscriptions as ub on ub.rockman_id = us.rockman_id
          WHERE """ <> timestampFiltersToSqlWhere contextUserSessionsRedshift params <> """
          AND """ <> toSql params contextUserSessionsRedshift filters <> """
          GROUP BY """ <> breakdownToSqlGroupBy contextUserSessionsRedshift breakdowns <> """
        """

        -- send {filters: filters, timestampWhere: timestampWhere}

        r <- liftAff $ attempt $ state.querySync false query query
        case r of
            Right result -> send result
            Left ex -> do setStatus 500  
                          send {error: show ex}

    get "/*" $ sendFile (pathResolve "./src/index.html") (const $ pure unit)

connect :: String → Effect AppState
connect connStr = do
  Console.log "connecting ..."
  
  let connectionInfo = PG.connectionInfoFromString connStr

  cache <- Ref.new mkQueryCache
  queryPool <- PG.mkPool connectionInfo
  adminPool <- PG.mkPool connectionInfo

  let queryAsync' = queryAsync cache {queryPool, adminPool} 
  let querySync' = querySync cache {queryPool, adminPool}  
  let getQueryState' = getQueryState cache
  let killQuery' = killQuery cache

  let state = AppState { 
          queryCache: cache
        , queryAsync: queryAsync'
        , getQueryState: getQueryState'  
        , killQuery: killQuery'
        , querySync: querySync'
        }

  Console.log "pool created"  

  pure state



main :: Effect Server
main = do
    port <- (parseInt <<< fromMaybe "8080") <$> lookupEnv "PORT"
    listenHttp app port \_ ->
        log $ "Listening on " <> show port

parseInt :: String -> Int
parseInt str = fromMaybe 0 $ fromString str


---


contextUserSessionsRedshift :: QueryContext
contextUserSessionsRedshift = {
  timestampColumn : {
    name: "us.\"timestamp\"",
    timezone: WithoutTimezone (toNumber 0)
  },
  tableAlias : "us",
  fieldMap : Map.fromFoldable [
    Tuple "screen_size" (Expr "coalesce(cast(round(us.screen_width/ 50) :: Int * 50 as varchar) || 'X' || cast(round(us.screen_height/ 50) :: Int * 50 as varchar), 'Unknown')")
  ],
  engine : Redshift
}