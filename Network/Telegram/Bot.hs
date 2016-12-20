{-# LANGUAGE RecordWildCards #-}


module Network.Telegram.Bot where

import           Control.Exception
import           Control.Monad.Catch        (MonadThrow)
import           Control.Monad.IO.Class     (MonadIO)
import           Data.Aeson
import           Data.ByteString            (ByteString)
import qualified Data.ByteString.Lazy       as LB
import qualified Data.HashMap.Strict        as HM
import           Data.Monoid                ((<>))
import           Data.Scientific            (floatingOrInteger)
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as TE
import qualified Data.Text.Lazy.Encoding    as TLE
import           Data.Typeable
import           Network.HTTP.Client.MultipartFormData
import           Network.HTTP.Conduit
import           Network.HTTP.Types         (hContentType)

import qualified Web.Telegram.Bot           as TG

import           Network.Telegram.Bot.Types



telegramPostFileRequest :: (MonadIO m, MonadThrow m, ToJSON a, FromJSON b) =>
                       Token -> String -> FilePath -> [(ByteString,Maybe ByteString)] -> a -> Manager -> m (Either TelegramBadResponse (TG.Response b))
telegramPostFileRequest token url path querystring a mngr = do
  request <- goPR token url
  case toJSON a of
    Object o -> do
      partlist <- mapM valueToPart $ HM.toList o
      flip goHTTP mngr =<< formDataBody partlist request
    _ -> throw NotMultipartable
 where
  valueToPart :: (MonadIO m, MonadThrow m) => (Text,Value) -> m Part
  valueToPart (t,Number n)   | Right i <- floatingOrInteger n = return $ partBS t $ TE.encodeUtf8 . T.pack $ show i
                             | otherwise = return $ partBS t $ TE.encodeUtf8 . T.pack $ show n
  valueToPart (t,Bool True)  = return $ partBS t "true"
  valueToPart (t,Bool False) = return $ partBS t "false"
  valueToPart (t,String s)   | t `elem` inputFileFields
                             , T.take 1 s == "/"
                                = fileFromPath t $ T.unpack s
                             | t `elem` inputFileFields
                             , (T.take 7 s == "http://" || T.take 8 s == "https://")
                                = return $ fileFromUrl t $ T.unpack s
                             | otherwise = return $ partBS t $ TE.encodeUtf8 s
  valueToPart (t,v)          = return $ partLBS t $ encode v
  fileFromPath name path = let part = (partFileSource name path)
                           in return $ part { partFilename = getNameFromPath <$> partFilename part }
  fileFromUrl name url   = partFileRequestBodyM name (getNameFromPath url) $ do
    fmap (RequestBodyLBS . responseBody) . flip httpLbs mngr =<< parseRequest url
  getNameFromPath s = if dropWhile (/= '/') s == []
                        then s
                        else getNameFromPath $ drop 1 s

inputFileFields :: [Text]
inputFileFields = ["audio","photo","document","sticker","video","voice","certificate"]

data TelegramException = NotMultipartable deriving (Show, Typeable)
instance Exception TelegramException


telegramPostRequest :: (MonadIO m, MonadThrow m, ToJSON a, FromJSON b) =>
                       Token -> String -> [(ByteString,Maybe ByteString)] -> a -> Manager -> m (Either TelegramBadResponse (TG.Response b))
telegramPostRequest token url querystring a mngr = do
  req' <- goPR token url
  let req = req' { method = "POST"
                 , requestBody = RequestBodyLBS $ encode a
                 , requestHeaders = [(hContentType,"application/json")]
                 }
      request = flip setQueryString req querystring
  goHTTP request mngr


telegramGetRequest :: (MonadIO m, MonadThrow m, FromJSON a) =>
                       Token -> String -> [(ByteString,Maybe ByteString)] -> Manager -> m (Either TelegramBadResponse (TG.Response a))
telegramGetRequest token url querystring mngr = do
  req <- goPR token url
  let request = flip setQueryString req querystring
  goHTTP request mngr 


goPR :: (Monad m, MonadThrow m) => Token -> String -> m Request
goPR token url = parseRequest $ "https://api.telegram.org/bot" <> T.unpack token <> "/" <> url

goHTTP :: (MonadIO m, MonadThrow m, FromJSON b) =>
          Request -> Manager -> m (Either TelegramBadResponse (TG.Response b))
goHTTP req mngr = do
    res <- httpLbs req mngr
    let response = responseBody res
    case eitherDecode' response of
        Right res2    -> return $ Right res2
        Left firsterr -> return $ Left $ TelegramBadResponse (T.pack firsterr) $ LB.toStrict response
