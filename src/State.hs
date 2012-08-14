module State (State,newS,readS,writeS) where
import Control.Concurrent (MVar, newMVar, newEmptyMVar, modifyMVar_, readMVar)
import Data.Text (Text)

type State = MVar [Text]

newS :: IO State
newS = do
    v <- newMVar []
    return v

readS :: State -> IO [Text]
readS s = do
    v <- readMVar s
    return v

writeS :: [Text] -> State -> IO ()
writeS msg s = modifyMVar_ s $ \_ -> return (msg)

{-
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
incCountBy n =
    do c@CounterState{..} <- get
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
dbServer = do bracket (openLocalState initialCounterState)
           (createCheckpointAndClose)
           (\acid -> simpleHTTP nullConf (handlers acid))
-}
