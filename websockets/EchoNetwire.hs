{-# LANGUAGE OverloadedStrings #-}

import Prelude hiding (id, (.))
import Control.Concurrent
import Control.Concurrent.Chan
import Data.Char (isPunctuation, isSpace)
import Data.Monoid (mappend)
import Data.Text (Text)
import Control.Exception (fromException)
import Control.Monad (forM_, forever)
import Control.Monad.IO.Class
import Control.Concurrent (MVar, newMVar, modifyMVar_, readMVar)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Network.WebSockets
import Control.Wire
import Control.Wire.Prefab

meowTime :: (Monad m) => Wire e m Text Text
meowTime = liftA2 mappend (arr (T.pack . show) . time) id

type Queue = Chan (Text, Chan Text)

chanWire :: Queue -> Session IO -> Wire e IO Text Text -> IO a
chanWire chan = loop
  where
    loop session w = do
        (i, ochan) <- readChan chan
        (mx, w', session') <- stepSession w session i
        either (const $ return ()) (writeChan ochan) mx
        loop session' w'

meow :: TextProtocol p => WebSockets p ()
meow = forever $ do
    msg <- receiveData
    sendTextData $ msg `T.append` ", meow."

wsWire :: (TextProtocol p)
    => Session IO
    -> Wire e IO Text Text
    -> WebSockets p ()
wsWire = loop
  where
    loop session w = do
        msg <- receiveData
        (mx, w', session') <- liftIO $ stepSession w session msg
        either (\_ -> return ()) sendTextData mx
        loop session' w'

wsQueue :: (TextProtocol p) => Request -> Queue -> WebSockets p ()
wsQueue _ q = do
    sink <- getSink
    ochan <- liftIO newChan
    liftIO $ forkIO $ forever (readChan ochan >>= sendSink sink . textData)
    forever (receiveData >>= \x -> liftIO $ writeChan q (x, ochan))

--application :: Request -> WebSockets Hybi00 ()
--application req = acceptRequest req >> wsWire clockSession meowTime

main :: IO ()
main = do
    queue <- newChan
    let application :: Request -> WebSockets Hybi00 ()
        application req = acceptRequest req >> wsQueue req queue
    forkIO $ chanWire queue clockSession meowTime
    runServer "0.0.0.0" 8085 application

--runServer "0.0.0.0" 8085 application
