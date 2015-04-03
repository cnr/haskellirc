
module Main where

import IRC.Bot
import IRC.Parser

main :: IO ()
main = makeBot esper

esper :: BotConfig
esper = BotConfig { botHost = "availo.esper.net"
                  , botPort = 6667
                  , botParts = [basics, printI]
                  }

basics :: BotHandler
basics msg = case msg of
      Ping p        -> writeMsg (RawIRCMessage Nothing "PONG" [p])
      RPL_Welcome _ -> writeMsg (RawIRCMessage Nothing "JOIN" ["#e-test"])
      PrivMsg s t m -> writeMsg (RawIRCMessage Nothing "PRIVMSG" ["#e-test", reverse m])
      _             -> return ()
