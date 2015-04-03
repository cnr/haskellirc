
module IRC.Bot
  ( Bot(..)
  , BotConfig(..)
  , BotHandler
  , askHost
  , askPort
  , askHandle
  , makeBot
  , writeMsg
  , logI
  , printI
  ) where

import           Control.Monad (forever, mapM_)
import           Control.Monad.Reader
import           Data.Attoparsec.Text.Lazy
import qualified Data.Text.Lazy as T
import           Data.Text.Lazy.IO hiding (putStrLn)
import           Network.Simple.TCP (connect, send)
import           Network.Socket (Socket(..), socketToHandle)
import           System.IO (Handle(..), IOMode(ReadWriteMode))

import IRC.Parser

type Bot a = ReaderT BotEnv IO a

type BotEnv = (String, Int, Handle) -- Host, Port, Handle

data BotConfig = BotConfig { botHost :: String
                           , botPort :: Int
                           , botParts :: [BotHandler]
                           }

type BotHandler = IRCMessage -> Bot ()

askHost :: Bot String
askHost = ask >>= \(host,_,_) -> return host

askPort :: Bot Int
askPort = ask >>= \(_,port,_) -> return port

askHandle :: Bot Handle
askHandle = ask >>= \(_,_,handle) -> return handle

writeMsg :: RawIRCMessage -> Bot ()
writeMsg msg = do
    handle <- askHandle
    liftIO $ hPutStr handle $ T.pack (msgToString msg)

logI :: String -> Bot ()
logI = liftIO . putStrLn

printI :: Show a => a -> Bot ()
printI = liftIO . print

makeBot :: BotConfig -> IO ()
makeBot (BotConfig addr port parts) = connect addr (show port) $ \(socket, address) -> do
      handle <- liftIO $ socketToHandle socket ReadWriteMode

      let env = (addr, port, handle) :: BotEnv

      runBot env startConnection
      runBot env (readMessages handle parts)

readMessages :: Handle -> [BotHandler] -> Bot ()
readMessages handle parts = do
    contents <- liftIO $ hGetContents handle
    go parts contents
      where
        go :: [BotHandler] -> T.Text -> Bot ()
        go parts t = case parse ircParser t of
                         Done t r   -> do
                             let msg = rawToIRCMessage r
                             env <- ask
                             liftIO $ mapM_ (runBot env . ($ msg)) parts
                             go parts t
                         Fail r _ _ -> liftIO $ print t >> print r

startConnection :: Bot ()
startConnection = do
    writeMsg (RawIRCMessage Nothing "NICK" ["e-bot"])
    writeMsg (RawIRCMessage Nothing "USER" ["e-bot", "0", "*", "e-bot"])

runBot :: BotEnv -> Bot () -> IO ()
runBot = flip runReaderT

process = undefined
