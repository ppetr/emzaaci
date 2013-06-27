{-# LANGUAGE OverloadedStrings #-}

import Data.Char (isPunctuation, isSpace)
import Data.Monoid (mappend)
import Data.Text (Text)
import Control.Exception (fromException)
import Control.Monad (forM_, forever)
import Control.Concurrent (MVar, newMVar, modifyMVar_, readMVar)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Network.WebSockets

meow :: TextProtocol p => WebSockets p ()
meow = forever $ do
    msg <- receiveData
    sendTextData $ msg `T.append` ", meow."

app :: Request -> WebSockets Hybi00 ()
app req = acceptRequest req >> meow

main :: IO ()
main = runServer "0.0.0.0" 8085 app
