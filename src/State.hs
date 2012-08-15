module State (State,newS,readS,writeS) where
import Control.Concurrent (MVar, newMVar, modifyMVar_, readMVar)
import Data.Text (Text, pack)
import Facebook (UserId)

type State = MVar [(UserId,Text)]

newS :: IO State
newS = do
    s <- newMVar []
    return s

readS :: UserId -> State -> IO (Text)
readS i s = do
    xs <- readMVar s
    let e = lookup i xs
    case e of
        Just e -> return (e)
        _ -> return (pack "{}")

delete :: Eq a => a -> [(a,b)] -> [(a,b)]
delete i xs = [x|x<-xs,(fst x)/= i]

writeS :: UserId -> Text -> State -> IO ()
writeS i m s = modifyMVar_ s $ \xs -> do
    let xs' = delete i xs
    return ((i,m):xs')

{-
readS :: UserId -> State -> IO (Text)
readS i s = do return (pack ("{\"city\":\"test\",\"country\":\"test\",\"phone\":\"test\",\"email\":\"test\"}"))

{-# LANGUAGE CPP, DeriveDataTypeable, FlexibleContexts, GeneralizedNewtypeDeriving,
    MultiParamTypeClasses, TemplateHaskell, TypeFamilies, RecordWildCards #-}
import Control.Applicative  ( (<$>) )
import Control.Exception    ( bracket )
import Control.Monad        ( msum )
import Control.Monad.Reader ( ask )
import Control.Monad.State  ( get, put )
import Data.Data            ( Data, Typeable )
import Happstack.Server     ( Response, ServerPart, dir, nullDir, nullConf, ok, simpleHTTP, toResponse )
import Data.Acid            ( AcidState, Query, Update, makeAcidic, openLocalState )
import Data.Acid.Advanced   ( query', update' )
import Data.Acid.Local      ( createCheckpointAndClose )
import Data.SafeCopy        ( base, deriveSafeCopy )

data CounterState = CounterState { count :: Integer }
    deriving (Eq, Ord, Read, Show, Data, Typeable)

$(deriveSafeCopy 0 'base ''CounterState)

initialCounterState :: CounterState
initialCounterState = CounterState 0

incCountBy :: Integer -> Update CounterState Integer
incCountBy n = do
    c@CounterState{..} <- get
    let newCount = count + n
    put $ c { count = newCount }
    return newCount

peekCount :: Query CounterState Integer
peekCount = count <$> ask

$(makeAcidic ''CounterState ['incCountBy, 'peekCount])

handlers :: AcidState CounterState -> ServerPart Response
handlers acid =
    msum [ dir "peek" $ do c <- query' acid PeekCount
                           ok $ toResponse $ "peeked at the count and saw: " ++ show c
         , do nullDir
              c <- update' acid (IncCountBy 1)
              ok $ toResponse $ "New count is: " ++ show c ]

update' :: (UpdateEvent event, MonadIO m) =>
           AcidState (EventState event) -- ^ handle to acid-state
        -> event                        -- ^ update event to execute
        -> m (EventResult event)

query'  :: (QueryEvent event , MonadIO m) =>
           AcidState (EventState event) -- ^ handle to acid-state
        -> event                        -- ^ query event to execute
        -> m (EventResult event)

data PeekCount  = PeekCount
data IncCountBy = IncCountBy Integer

dbServer :: IO ()
dbServer = do
    bracket (openLocalState initialCounterState)
    (createCheckpointAndClose)
    (\acid -> simpleHTTP nullConf (handlers acid))
-}
