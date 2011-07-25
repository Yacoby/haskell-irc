--because sometimes specifing types is a pita
{-# LANGUAGE NoMonomorphismRestriction  #-}

import Network
import System.IO

import Control.Monad.State
import Control.Monad.Reader
import Data.Functor.Identity

import Data.List

import Text.ParserCombinators.Parsec

-- -----------------------------------------------------------------------------
-- constants
server = "irc.imaginarynet.org.uk"
port   = 6667
nick   = "KarmaBot"
user   = "KarmaBot"
name   = "Yacoby v2"

-- -----------------------------------------------------------------------------

data IrcStream = IrcStream { irc :: Handle } 

type IrcLogger = ReaderT IrcStream IO

main :: IO ()
main  = do
    h <- connect server port nick user
    runBot ircOutput h

ircOutput :: Response -> IrcLogger ()
ircOutput (Response _ (IntCmd "004" _ msg)) = write "JOIN #yac"
ircOutput _                                 = return ()

-- -----------------------------------------------------------------------------
-- Lower level irc stuff

runBot :: (Response -> IrcLogger ()) -> Handle -> IO ()
runBot output handle = runReaderT (mainLoop output) (IrcStream handle)

connect :: String -> Int -> String -> String -> IO Handle
connect server port nick name = do
    h <- connectTo server $ PortNumber $ fromIntegral port
    hSetBuffering h NoBuffering
    hPutStrLn h $ "NICK " ++ nick
    hPutStrLn h $ "USER " ++ nick ++ " 0 * :" ++ name
    return h

mainLoop :: (Response -> IrcLogger () ) -> IrcLogger ()
mainLoop outfunc = forever $ do
    h <- asks irc
    line <- liftIO $ hGetLine h
    liftIO $ putStrLn line
    maybeDispatch outfunc $ parseMessage line
    where
        forever a = a >> forever a

maybeDispatch :: (Response -> IrcLogger() ) -> Maybe Response -> IrcLogger ()
maybeDispatch outputf response = maybe deflt processMessage response
    where
        --default, general debug
        deflt = liftIO $ putStrLn "++PARSE FAIL++"

        --we don't need to see the pong any higher up, there is no point
        --as it just adds complexity to higher levels
        processMessage :: Response -> IrcLogger ()
        processMessage (Response _ (Ping  m)) = write $ "PONG :" ++ m
        processMessage response               = outputf response

write :: String -> IrcLogger ()
write msg = do
    h <- asks irc
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

cmdPrivateMsg = do
    string "PRIVMSG "
    dest <- many $ noneOf " "
    string " :"
    msg <- many anyChar
    return $ PrivateMsg dest msg

cmdPing = do
    string "PING :"
    msg <- many alphaNum
    return $ Ping msg

cmdInt = do
    num <- many digit
    char ' '
    dest <- many (noneOf " ")
    char ' '
    rest <- many anyChar
    return $ IntCmd num dest rest

cmdNotice = do
    string "NOTICE "
    nick <- many (noneOf " ")
    char ' '
    text <- many anyChar
    return $ Notice nick text

cmd = try cmdNotice <|>
      try cmdPing <|>
      try cmdInt  <|>
      cmdPrivateMsg

cmdMsg = do
    host <- optionMaybe hostMask
    msg <- cmd
    return $ Response host msg


parseMessage :: String -> Maybe Response
parseMessage msg = case parse cmdMsg "" msg of
                    Left e  -> Nothing
                    Right r -> Just r
