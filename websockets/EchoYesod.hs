-- http://stackoverflow.com/a/15054127/1333025
{-# LANGUAGE QuasiQuotes, TypeFamilies, GeneralizedNewtypeDeriving, TemplateHaskell, OverloadedStrings, GADTs, FlexibleContexts #-}
module Main where
import Control.Monad.IO.Class (liftIO)
import Data.String (fromString)
import Network.Wai.Application.Static (staticApp, defaultWebAppSettings, defaultFileServerSettings)
import Network.Wai.Handler.Warp (runSettings, defaultSettings, settingsIntercept, settingsPort)
import Network.Wai.Handler.WebSockets (intercept)
import qualified Network.WebSockets as WS

meow :: TextProtocol p => WS.Request -> WebSockets p ()
meow _ = forever $ do
    msg <- receiveData
    sendTextData $ msg `T.append` ", meow."

{-
ws :: WS.Request -> WS.WebSockets WS.Hybi10 ()
ws r = do
    WS.acceptRequest r
    liftIO $ runSqlite ":memory:" $ do
        runMigration migrateAll
        michaelId <- insert $ Person "Michael" 26
        michael <- get michaelId
        liftIO $ print michael
-}

main :: IO ()
main = runSettings defaultSettings
    { settingsPort = 8085
    , settingsIntercept = intercept $ meow
    } $ staticApp (defaultFileServerSettings $ fromString ".")
