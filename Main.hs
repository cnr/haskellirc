
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Prelude hiding (hPutStr)

import Control.Monad
import Data.Attoparsec.Text.Lazy
import qualified Data.Text.Lazy as T
import Data.Text.Lazy.IO
import Network
import System.IO (Handle(..), BufferMode(NoBuffering), hSetBuffering)

import IRC.Parser

main :: IO ()
main = do
    h <- connectTo "availo.esper.net" (PortNumber 6667)
    hSetBuffering h NoBuffering
    hPutStr h (T.pack . msgToString $ RawIRCMessage Nothing "NICK" ["e-bot"])
    hPutStr h (T.pack . msgToString $ RawIRCMessage Nothing "USER" ["e-bot", "0", "*", "e-bot"])
    contents <- hGetContents h
    process h contents

process :: Handle -> T.Text -> IO ()
process h t = case parse ircParser t of
                  Done t r -> handle h r >> process h t
                  Fail r _ _ -> print t >> print r

handle :: Handle -> RawIRCMessage -> IO ()
handle h raw = case (rawToIRCMessage raw) of
    Just (Ping p)        -> hPutStr h (T.pack . msgToString $ RawIRCMessage Nothing "PONG" [p])
    Just (RPL_Welcome _) -> hPutStr h (T.pack . msgToString $ RawIRCMessage Nothing "JOIN" ["#e-test"])
    Just (PrivMsg s t m) -> hPutStr h (T.pack . msgToString $ RawIRCMessage Nothing "PRIVMSG" ["#e-test", m])
    Just msg -> print msg
    Nothing  -> print raw


---- testing....

type Bot = IO ()

readMsg :: IO IRCMessage
readMsg = undefined

handleraw :: (IRCMessage -> Bot) -> Bot
handleraw = (readMsg >>=)

privmsg :: (String -> String -> String -> Bot) -> Bot
privmsg action =
    readMsg >>= \case
        PrivMsg sender target message -> action sender target message
        _                             -> return ()

topicchange :: (String -> String -> Bot) -> Bot
topicchange action =
    readMsg >>= \case
        RPL_Topic channel topic -> action channel topic
        _                       -> return ()
