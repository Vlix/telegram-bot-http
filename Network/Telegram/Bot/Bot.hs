{-# LANGUAGE RecordWildCards #-}

module Network.Telegram.Bot where


import qualified Web.Telegram.Bot           as TG

import           Network.Telegram.Bot.Types

telegramPostRequest :: Token -> String -> [(ByteString,Maybe ByteString)] -> a -> Manager -> (CustomResponseType a ErrorRes)
telegramPostRequest token url querystring a mngr = do
  req' <- goPR token url
  let req = req' { method = "POST"
                 , requestBody = RequestBodyLBS $ encode a
                 , requestHeaders = [(hContentType,"application/json")]
                 }
      request = flip setQueryString req $ accessTokenQuery token : querystring
  goHTTP request mngr

goPR :: (Monad m, MonadThrow m) => Token -> String -> m Request
goPR token url = parseRequest $ "https://api.telegram.org/bot" <> unpack token <> "/" <> url

goHTTP :: (MonadIO m, MonadThrow m, FromJSON b) => Request -> Manager -> m (Either TelegramBadResponse (TG.Response b))
goHTTP req mngr = do
    res <- httpLbs req mngr
    let response = responseBody res
    case eitherDecode' response of
        Right res2    -> return $ Right res2
        Left firsterr -> return $ TelegramBadResponse firsterr res
