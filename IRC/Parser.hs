-- IRC message parser compliant with RFC 2812 https://tools.ietf.org/html/rfc2812

module IRC.Parser
  ( RawIRCMessage(..)
  , ircParser
  , IRCMessage(..)
  , rawToIRCMessage
  , msgToString
  ) where

import           Control.Applicative
import           Data.Char (isAlphaNum)
import           Data.Maybe (catMaybes, fromJust)
import qualified Data.Text as T
import           Data.Attoparsec.Combinator
import           Data.Attoparsec.Text.Lazy


data RawIRCMessage = RawIRCMessage { rawPrefix  :: Maybe String
                                   , rawCommand :: String
                                   , rawParams  :: [String]
                                   } deriving (Eq, Show)

msgToString :: RawIRCMessage -> String
msgToString (RawIRCMessage rpre rcmd rprms) = fmtPrefix rpre ++ rcmd ++ fmtParams rprms ++ "\r\n"
        where
            fmtPrefix (Just pre) = ':':pre ++ " "
            fmtPrefix _          = ""

            fmtParams []     = ""
            fmtParams [x]    = " :" ++ x
            fmtParams (x:xs) = " " ++ x ++ fmtParams xs

-- The `many space` before crlf is required to parse MODE messages from some networks
ircParser :: Parser RawIRCMessage
ircParser = RawIRCMessage <$> optional prefix <*> command <*> params <* many space <* crlf
        where
            prefix  = char ':' *> manyTill anyChar space
            command = many1 alphaNum
            params  = do
                leading <- count 14 (optional (try (space *> middle)))
                tail    <- optional (space *> trailing)
                return (catMaybes (leading ++ [tail]))
            
            middle = (:) <$> nospcrlfcl <*> many (char ':' <|> nospcrlfcl)

            trailing = char ':' *> many1 (oneOf ": " <|> nospcrlfcl)

            nospcrlfcl :: Parser Char
            nospcrlfcl = noneOf "\0\r\n :"

            space = char ' '
            crlf = string $ T.pack "\r\n"


-- Combinators from Parsec that don't exist in Attoparsec
alphaNum :: Parser Char
alphaNum = satisfy isAlphaNum

oneOf :: String -> Parser Char
oneOf xs = satisfy (`elem` xs)

noneOf :: String -> Parser Char
noneOf xs = satisfy (`notElem` xs)


-- Translation to standard IRC messages

data IRCMessage = Join String
                | Mode [String]
                | Notice String String String -- Sender, Targets, Message
                | Ping String
                | PrivMsg String String String -- Sender, Targets, Message
      {- 001 -} | RPL_Welcome String
      {- 002 -} | RPL_YourHost String
      {- 003 -} | RPL_Created String
      {- 004 -} | RPL_MyInfo [String]
      {- 005 -} | RPL_ISupport [String]
      {- 250 -} | RPL_StatsConn String
      {- 251 -} | RPL_LUserClient String
      {- 252 -} | RPL_LUserOp Int -- Number of operators online
      {- 254 -} | RPL_LUserChannels Int -- Number of channels formed
      {- 255 -} | RPL_LUserMe String -- "I have X clients and X servers"
      {- 265 -} | RPL_LocalUsers Int Int -- Current, Maximum users
      {- 266 -} | RPL_GlobalUsers Int Int -- Current, Maximum users
      {- 332 -} | RPL_Topic String String -- Channel, Topic
      {- 333 -} | RPL_TopicWhoTime String String -- Channel, User Mask
      {- 353 -} | RPL_NamReply [String]
      {- 366 -} | RPL_EndOfNames String String -- Channel, Info
      {- 372 -} | RPL_Motd String
      {- 375 -} | RPL_MotdStart String
      {- 376 -} | RPL_EndOfMotd String
                | Unknown String
                deriving (Show)

rawToIRCMessage :: RawIRCMessage -> IRCMessage
rawToIRCMessage msg = case (rawCommand msg, rawParams msg) of
        ("JOIN"   , [c]       ) -> Join c
        ("MODE"   , xs        ) -> Mode xs
        ("NOTICE" , [t,m]     ) -> Notice (fromJust (rawPrefix msg)) t m
        ("PING"   , [p]       ) -> Ping p
        ("PRIVMSG", [t,m]     ) -> PrivMsg (fromJust (rawPrefix msg)) t m
        ("001"    , [_,w]     ) -> RPL_Welcome w
        ("002"    , [_,h]     ) -> RPL_YourHost h
        ("003"    , [c]       ) -> RPL_Created c
        ("004"    , _:xs      ) -> RPL_MyInfo xs
        ("005"    , xs        ) -> RPL_ISupport xs
        ("250"    , [_,s]     ) -> RPL_StatsConn s
        ("251"    , [_,m]     ) -> RPL_LUserClient m
        ("252"    , [_,n,_]   ) -> RPL_LUserOp (read n)
        ("254"    , [_,n,_]   ) -> RPL_LUserChannels (read n)
        ("255"    , [_,m]     ) -> RPL_LUserMe m
        ("265"    , [_,c,m,_] ) -> RPL_LocalUsers (read c) (read m)
        ("266"    , [_,c,m,_] ) -> RPL_GlobalUsers (read c) (read m)
        ("332"    , [_,c,t]   ) -> RPL_Topic c t
        ("333"    , [_,c,u]   ) -> RPL_TopicWhoTime c u
        ("353"    , _:xs      ) -> RPL_NamReply xs
        ("366"    , [_,c,m]   ) -> RPL_EndOfNames c m
        ("372"    , [_,m]     ) -> RPL_Motd m
        ("375"    , [_,m]     ) -> RPL_MotdStart m
        ("376"    , [_,m]     ) -> RPL_EndOfMotd m
        (_,_)                   -> Unknown (show msg)
