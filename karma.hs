{-# LANGUAGE NoMonomorphismRestriction #-}

import Network
import System.IO

import Control.Monad.State
import Control.Monad.Reader
import Data.Functor.Identity

import Data.List

import Text.ParserCombinators.Parsec

-- constants
server = "irc.imaginarynet.org.uk"
port = 6667


data IrcStream = IrcStream { irc :: Handle } 

type IrcLogger = ReaderT IrcStream IO


main :: IO ()
main  = do
    h <- connect
    hPutStrLn h "NICK TEST"
    hPutStrLn h "USER TestUsr 0 * :YacobyIsAwesome"
    runReaderT runBot (IrcStream h)
    return ()

runBot :: IrcLogger ()
runBot = forever $ do
    h <- asks irc
    line <- liftIO (hGetLine h)
    liftIO $ putStrLn line
    msgDispatch (parseMessage line)
    return ()
    where
        forever a = a >> forever a

msgDispatch :: Maybe Message -> IrcLogger ()
msgDispatch Nothing  = liftIO $ putStrLn "++PARSE FAIL++"  --return ()
msgDispatch (Just m) = processMessage m

joinChan = do write "JOIN #yac"

dealWithMsg :: Message -> IrcLogger ()
dealWithMsg (Msg host "004" to msg)     = joinChan
dealWithMsg (Msg host "PRIVMSG" to msg) = liftIO $ putStrLn ( "<< " ++ msg )
dealWithMsg _                           = return ()

processMessage :: Message -> IrcLogger ()
processMessage (Ping   m)   = write ("PONG :" ++ m) --catch this, as never needed
processMessage m            = dealWithMsg m

write :: String -> IrcLogger ()
write msg = do
    h <- asks irc
    liftIO $ hPutStrLn h msg
    liftIO $ putStrLn $ ">>" ++ msg

connect :: IO Handle
connect = do
    h <- connectTo server $ PortNumber $ fromIntegral port
    hSetBuffering h NoBuffering
    return h
-- -----------------------------------------------
-- Simple mesage parser, very basic

data Message = Ping String
             | Notice
             | Msg String String String String
             deriving (Show)

pingMsg = do
    string "PING :"
    msg <- many alphaNum
    return $ Ping msg

noticeMsg = do
    string "NOTICE"
    return $ Notice

housekeepingMsg = (pingMsg <|> noticeMsg)

visibleMessage = do
    char ':'
    host <- many (noneOf " ")
    char ' '
    cmd <- many alphaNum
    char ' '
    to <- many (noneOf " ")
    try (string " :") <|> (string " ")
    rest <- many anyChar

    return $ Msg host cmd to rest

parseMessage :: String -> Maybe Message
parseMessage msg = case parse (visibleMessage <|> housekeepingMsg) "" msg of
                    Left e  -> Nothing
                    Right r -> Just r
-- -----------------------------------------------

