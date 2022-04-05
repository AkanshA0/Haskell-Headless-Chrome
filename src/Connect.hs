{-# LANGUAGE OverloadedStrings #-}

module Connect
   ( main
   ) where
import           Control.Applicative  ((<$>))
import           Control.Monad        (forever, mzero)
import           Control.Monad.Trans  (liftIO)
import           Data.Aeson           (FromJSON (..), ToJSON (..), (.:), (.=))
import qualified Data.Aeson           as A
import qualified Data.Map             as M
import           Data.Maybe           (fromMaybe)
import qualified Data.Text.IO         as T
import qualified Network.HTTP.Conduit as Http
import qualified Network.URI          as Uri
import qualified Network.WebSockets   as WS
import Network.HTTP.Simple
import System.Process
import System.Directory
import qualified Data.ByteString.Lazy as B


data ChromiumPageInfo = ChromiumPageInfo
    { chromiumDebuggerUrl :: String
    } deriving (Show)
instance FromJSON ChromiumPageInfo where
    parseJSON (A.Object obj) =
        ChromiumPageInfo <$> obj .: "webSocketDebuggerUrl"
    parseJSON _              = mzero

getChromiumPageInfo :: IO [ChromiumPageInfo]
getChromiumPageInfo = do
    req <- Http.parseUrl "http://127.0.0.1:9222/json"
    response <- httpLbs req
    
    case A.decode (Http.responseBody response) of
        Nothing -> error "getChromiumPageInfo: Parse error"
        Just ci -> return ci

data PdfData = PdfData
    { dataP :: String
    } deriving (Show)
instance FromJSON PdfData where
    parseJSON (A.Object obj) =
        PdfData <$> obj .: "result"
    parseJSON _              = mzero

getPdfData :: B.ByteString -> IO [PdfData]
getPdfData msg = do
    case A.decode (msg) of
         Nothing -> error "pdfData: Parse error"
         Just pd -> return pd

parseUri :: String -> (String, Int, String)
parseUri uri = fromMaybe (error "parseUri: Invalid URI") $ do
    u    <- Uri.parseURI uri
    auth <- Uri.uriAuthority u
    let port = case Uri.uriPort auth of (':' : str) -> read str; _ -> 80
    return (Uri.uriRegName auth, port, Uri.uriPath u)


data Command = Command
    { commandId     :: Int
    , commandMethod :: String
    , commandParams :: [(String, String)]
    } deriving (Show)

instance ToJSON Command where
    toJSON cmd = A.object
        [ "id"     .= commandId cmd
        , "method" .= commandMethod cmd
        , "params" .= M.fromList (commandParams cmd)
        ]

main :: IO ()
main = do
    (ci : _) <- getChromiumPageInfo
    print ci
    let (host, port, path) = parseUri (chromiumDebuggerUrl ci)
    print host
    print port
    print path
    WS.runClient host port path $ \conn -> do
        
        WS.sendTextData conn $ A.encode $ Command
            { commandId     = 1
            , commandMethod = "Page.navigate"
            , commandParams = [("url", "https://google.com/")]
            }
   
        WS.sendTextData conn $ A.encode $ Command
            { commandId     = 2
            , commandMethod = "Page.printToPDF"
            , commandParams = [("", "")]
            }    

        forever $ do
            msg <- WS.receiveData conn
            liftIO $ T.putStrLn msg
