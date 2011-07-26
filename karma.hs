--because sometimes specifing types is a pita
{-# LANGUAGE NoMonomorphismRestriction  #-}

import Network
import System.IO

import Control.Monad.Reader hiding (join)

import Data.List

import Text.ParserCombinators.Parsec

-- -----------------------------------------------------------------------------
-- constants
server = "irc.imaginarynet.org.uk"
port   = 6667
nick   = "KarmaBot"
user   = "KarmaBot"
chan   = "yac"

-- -----------------------------------------------------------------------------

data Network = Network { socket :: Handle } 

type Net = ReaderT Network IO

main :: IO ()
main  = do
    h <- connect server port nick user
    runBot ircOutput h

ircOutput :: Response -> Net ()
ircOutput (Response _ (IntCmd "004" _ msg)) = join chan
ircOutput _                                 = return ()

-- ----------------------------------------------------------------------------
-- IRC Commands

join :: String -> Net ()
join chan = write $ "JOIN #" ++ chan

-- -----------------------------------------------------------------------------
-- Lower level irc stuff

runBot :: (Response -> Net ()) -> Handle -> IO ()
runBot output handle = runReaderT (mainLoop output) (Network handle)

connect :: String -> Int -> String -> String -> IO Handle
connect server port nick name = do
    h <- connectTo server $ PortNumber $ fromIntegral port
    hSetBuffering h NoBuffering
    hPutStrLn h $ "NICK " ++ nick
    hPutStrLn h $ "USER " ++ nick ++ " 0 * :" ++ name
    return h

mainLoop :: (Response -> Net()) -> Net()
mainLoop outfunc = forever $ do
    h <- asks socket 
    line <- liftIO $ hGetLine h
    liftIO $ putStrLn line
    maybeDispatch outfunc $ parseMessage line
    where
        forever a = a >> forever a

maybeDispatch :: (Response -> Net() ) -> Maybe Response -> Net()
maybeDispatch outputf response = maybe deflt processMessage response
    where
        --default, general debug
        deflt = liftIO $ putStrLn "++PARSE FAIL++"

        --we don't need to see the pong any higher up, there is no point
        --as it just adds complexity to higher levels
        processMessage :: Response -> Net()
        processMessage (Response _ (Ping  m)) = write $ "PONG :" ++ m
        processMessage response               = outputf response

write :: String -> Net()
write msg = do
    h <- asks socket 
    liftIO $ hPutStrLn h msg

    --debug
    liftIO $ putStrLn $ ">>" ++ msg

-- --------------------------------
-- Simple mesage parser, very basic

data Response = Response { host ::  Maybe Host,
                           message  :: Message }
                          deriving (Show, Eq)


data Host = Host { hostName :: String,
                   hostDetails :: String }
                  deriving (Show, Eq)

hostMask = do
    char ':'
    host <- many $ noneOf "! "
    other <- option "" parseRest
    return $ Host host other
    where
        parseRest = do 
            str <- many (noneOf " ")
            char ' '
            return str

-- -----------------------
-- Basic commands/messges
data Message = Ping String
             | Notice { nickname :: String, text :: String }
             | PrivateMsg  { destination :: String, contents :: String }
             | IntCmd { intCmd :: String, destination :: String, contents :: String }
             deriving (Show, Eq)


-- Parses commands, by this point the host has been removed so we just have
-- the command left
arg p = do
    a <- many1 p
    option "" ( string " " )
    return a

digitArg = arg  digit 
strArg = arg (noneOf " ")

remStrArg = many anyChar

cmdPrivateMsg = do
    string "PRIVMSG "
    liftM2 PrivateMsg strArg (string " :" >> remStrArg)

cmdPing = do
    string "PING :"
    liftM Ping remStrArg

cmdInt = liftM3 IntCmd digitArg strArg remStrArg

cmdNotice = do
    string "NOTICE "
    liftM2 Notice strArg remStrArg

tryCommands = foldl1 (<|>) . map try

parseMessage :: String -> Maybe Response
parseMessage msg = case parse parser "" msg of
                    Left e  -> Nothing
                    Right r -> Just r
            where
                commands = tryCommands [cmdNotice, cmdPing,
                                        cmdInt, cmdPrivateMsg]
                parser = liftM2 Response (optionMaybe hostMask) commands
