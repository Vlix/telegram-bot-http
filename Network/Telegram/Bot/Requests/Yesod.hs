module Network.Telegram.Bot.Requests.Yesod where


import           Control.Exception
import           Control.Monad.Catch        (MonadThrow)
import           Control.Monad.Reader       (MonadReader,asks)
import           Control.Monad.IO.Class     (MonadIO,liftIO)

import           Data.Aeson
import           Data.ByteString            (ByteString)
import qualified Data.ByteString            as B
import qualified Data.ByteString.Lazy       as LB
import qualified Data.HashMap.Strict        as HM
import           Data.Monoid                ((<>))
import           Data.Scientific            (floatingOrInteger)
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as TE
import qualified Data.Text.Lazy.Encoding    as TLE
import           Data.Typeable

import qualified Network.HTTP.Client        as CLIENT
import           Network.HTTP.Client.MultipartFormData
import           Network.HTTP.Client.Conduit
import           Network.HTTP.Types         (hContentType)

import qualified Web.Telegram.Bot           as TG

import           Network.Telegram.Bot.Types


type TelegramRequest a m b = a -> Token -> m (Either TelegramBadResponse (TG.Response b))

tshow :: Show a => a -> Text
tshow = T.pack . show


----------------------------
-- SEND AND GET FUNCTIONS --
----------------------------

getMeRequest :: (MonadThrow m, MonadIO m, HasHttpManager env, MonadReader env m) => Token -> m (Either TelegramBadResponse (TG.Response TG.User))
getMeRequest = telegramGetRequest "getMe" []

sendChatActionRequest :: (MonadThrow m, MonadIO m, HasHttpManager env, MonadReader env m) => TelegramRequest TG.SendChatActionRequest m TG.Message
sendChatActionRequest = telegramPostJSONRequest "sendChatAction" []

sendMessageRequest :: (MonadThrow m, MonadIO m, HasHttpManager env, MonadReader env m) => TelegramRequest TG.SendMessageRequest m TG.Message
sendMessageRequest = telegramPostJSONRequest "sendMessage" []

forwardMessageRequest :: (MonadThrow m, MonadIO m, HasHttpManager env, MonadReader env m) => TelegramRequest TG.ForwardMessageRequest m TG.Message
forwardMessageRequest = telegramPostJSONRequest "forwardMessage" []

answerCallbackQueryRequest :: (MonadThrow m, MonadIO m, HasHttpManager env, MonadReader env m) => TelegramRequest TG.AnswerCallbackQueryRequest m Bool
answerCallbackQueryRequest = telegramPostJSONRequest "answerCallbackQuery" []


sendPhotoRequest :: (MonadThrow m, MonadIO m, HasHttpManager env, MonadReader env m) => TelegramRequest TG.SendPhotoRequest m TG.Message
sendPhotoRequest = telegramPostJSONRequest "sendPhoto" []

sendAudioRequest :: (MonadThrow m, MonadIO m, HasHttpManager env, MonadReader env m) => TelegramRequest TG.SendAudioRequest m TG.Message
sendAudioRequest = telegramPostJSONRequest "sendAudio" []

sendDocumentRequest :: (MonadThrow m, MonadIO m, HasHttpManager env, MonadReader env m) => TelegramRequest TG.SendDocumentRequest m TG.Message
sendDocumentRequest = telegramPostJSONRequest "sendDocument" []

sendStickerRequest :: (MonadThrow m, MonadIO m, HasHttpManager env, MonadReader env m) => TelegramRequest TG.SendStickerRequest m TG.Message
sendStickerRequest = telegramPostJSONRequest "sendSticker" []

sendVideoRequest :: (MonadThrow m, MonadIO m, HasHttpManager env, MonadReader env m) => TelegramRequest TG.SendVideoRequest m TG.Message
sendVideoRequest = telegramPostJSONRequest "sendVideo" []

sendVoiceRequest :: (MonadThrow m, MonadIO m, HasHttpManager env, MonadReader env m) => TelegramRequest TG.SendVoiceRequest m TG.Message
sendVoiceRequest = telegramPostJSONRequest "sendVoice" []

sendLocationRequest :: (MonadThrow m, MonadIO m, HasHttpManager env, MonadReader env m) => TelegramRequest TG.SendLocationRequest m TG.Message
sendLocationRequest = telegramPostJSONRequest "sendLocation" []

sendVenueRequest :: (MonadThrow m, MonadIO m, HasHttpManager env, MonadReader env m) => TelegramRequest TG.SendVenueRequest m TG.Message
sendVenueRequest = telegramPostJSONRequest "sendVenue" []

sendContactRequest :: (MonadThrow m, MonadIO m, HasHttpManager env, MonadReader env m) => TelegramRequest TG.SendContactRequest m TG.Message
sendContactRequest = telegramPostJSONRequest "sendContact" []


getUserProfilePhotosRequest :: (MonadThrow m, MonadIO m, HasHttpManager env, MonadReader env m) =>
                            TelegramRequest TG.UserProfilePhotosRequest m TG.UserProfilePhotos
getUserProfilePhotosRequest = telegramPostJSONRequest "getUserProfilePhotos" []

getFileRequest :: (MonadThrow m, MonadIO m, HasHttpManager env, MonadReader env m) => TelegramRequest TG.FileRequest m TG.File
getFileRequest = telegramPostJSONRequest "getFile" []

downloadFileRequestToDisk :: (MonadThrow m, MonadIO m, HasHttpManager env, MonadReader env m) =>
                          FilePath -> TG.FileRequest -> Token -> m (Maybe (Text,Maybe TG.ResponseParameters))
downloadFileRequestToDisk saveTo req token = do
  tgFile <- getFileRequest req token
  case tgFile of
    Right (TG.OKResponse (TG.File _ _ (Just urlpath)) _) -> do
      req <- parseRequest $ "https://api.telegram.org/file/bot" <> T.unpack token <> "/" <> T.unpack urlpath
      mngr <- asks getHttpManager
      liftIO $ CLIENT.withResponse req mngr go
    Right (TG.OKResponse (TG.File _ _ Nothing) mdesc) -> return $ Just
      ("downloadFileRequestToDisk: NO URL GIVEN" <> maybe "" (" WITH DESC: " <>) mdesc,Nothing)
    Right (TG.ErrorResponse desc mcode mrp) -> return $ Just $
      ("downloadFileRequestToDisk: ERROR: " <> desc <> maybe "" (mappend " - CODE: " . tshow) mcode,mrp)
    Left (TelegramBadResponse parsefail _) -> return $ Just $ ("downloadFileRequestToDisk: BAD RESPONSE: " <> parsefail,Nothing)
 where
  go res = do
    b <- CLIENT.brRead bodyReader
    case b of
      "" -> return $ Just ("downloadFileRequestToDisk: EMPTY FILE",Nothing)
      _  -> B.writeFile saveTo b >> loop bodyReader
    where bodyReader = responseBody res
          loop breader = do
            br <- CLIENT.brRead breader
            case br of
              "" -> return $ Nothing
              _  -> B.appendFile saveTo br >> loop breader

-------------------
-- CHAT REQUESTS --
-------------------

kickChatMemberRequest :: (MonadThrow m, MonadIO m, HasHttpManager env, MonadReader env m) => TelegramRequest TG.KickChatMemberRequest m Bool
kickChatMemberRequest = telegramPostJSONRequest "kickChatMember" []

leaveChatRequest ::(MonadThrow m, MonadIO m, HasHttpManager env, MonadReader env m) => TelegramRequest TG.LeaveChatRequest m Bool
leaveChatRequest = telegramPostJSONRequest "leaveChat" []

unbanChatMemberRequest :: (MonadThrow m, MonadIO m, HasHttpManager env, MonadReader env m) => TelegramRequest TG.UnbanChatMemberRequest m Bool
unbanChatMemberRequest = telegramPostJSONRequest "unbanChatMember" []

getChatRequest :: (MonadThrow m, MonadIO m, HasHttpManager env, MonadReader env m) => TelegramRequest TG.GetChatRequest m TG.Chat
getChatRequest = telegramPostJSONRequest "getChat" []

getChatAdministratorsRequest :: (MonadThrow m, MonadIO m, HasHttpManager env, MonadReader env m) =>
                            TelegramRequest TG.GetChatAdministratorsRequest m [TG.ChatMember]
getChatAdministratorsRequest = telegramPostJSONRequest "getChatAdministrators" []

getChatMembersCountRequest :: (MonadThrow m, MonadIO m, HasHttpManager env, MonadReader env m) => TelegramRequest TG.GetChatMembersCountRequest m Int
getChatMembersCountRequest = telegramPostJSONRequest "getChatMembersCount" []

getChatMemberRequest :: (MonadThrow m, MonadIO m, HasHttpManager env, MonadReader env m) => TelegramRequest TG.GetChatMemberRequest m TG.ChatMember
getChatMemberRequest = telegramPostJSONRequest "getChatMember" []


---------------------
-- UPDATE REQUESTS --
---------------------

editMessageTextRequest :: (MonadThrow m, MonadIO m, HasHttpManager env, MonadReader env m) => TelegramRequest TG.EditMessageTextRequest m Bool
editMessageTextRequest = telegramPostJSONRequest "editMessageText" []

editMessageCaptionRequest :: (MonadThrow m, MonadIO m, HasHttpManager env, MonadReader env m) => TelegramRequest TG.EditMessageCaptionRequest m Bool
editMessageCaptionRequest = telegramPostJSONRequest "editMessageCaption" []

editMessageReplyMarkupRequest :: (MonadThrow m, MonadIO m, HasHttpManager env, MonadReader env m) => TelegramRequest TG.EditMessageReplyMarkupRequest m Bool
editMessageReplyMarkupRequest = telegramPostJSONRequest "editMessageReplyMarkupRequest" []


---------------------
-- INLINE REQUESTS --
---------------------

answerInlineQueryRequest :: (MonadThrow m, MonadIO m, HasHttpManager env, MonadReader env m) => TelegramRequest TG.EditMessageTextRequest m (Either Bool TG.Message)
answerInlineQueryRequest = telegramPostJSONRequest "answerInlineQuery" []


-------------------
-- GAME REQUESTS --
-------------------

sendGameRequest :: (MonadThrow m, MonadIO m, HasHttpManager env, MonadReader env m) => TelegramRequest TG.SendGameRequest m TG.Message
sendGameRequest = telegramPostJSONRequest "sendGame" []

setGameScoreRequest :: (MonadThrow m, MonadIO m, HasHttpManager env, MonadReader env m) => TelegramRequest TG.SetGameScoreRequest m (Either Bool TG.Message)
setGameScoreRequest = telegramPostJSONRequest "setGameScore" []

getGameHighScoresRequest :: (MonadThrow m, MonadIO m, HasHttpManager env, MonadReader env m) => TelegramRequest TG.GetGameHighScoresRequest m [TG.GameHighScore]
getGameHighScoresRequest = telegramPostJSONRequest "getGameHighScores" []


-------------------
-- UTIL REQUESTS --
-------------------

getUpdatesRequest :: (MonadThrow m, MonadIO m, HasHttpManager env, MonadReader env m) => TelegramRequest TG.UpdatesRequest m [TG.Update]
getUpdatesRequest = telegramPostJSONRequest "getUpdates" []

-- | DON'T USE THIS IF YOU WANT TO SEND A CERTIFICATE, USE setWebhookFileRequest IN THAT CASE
setWebhookRequest :: (MonadThrow m, MonadIO m, HasHttpManager env, MonadReader env m) => TelegramRequest TG.WebhookRequest m Bool
setWebhookRequest = telegramPostJSONRequest "setWebhook" []

deleteWebhookRequest :: (MonadThrow m, MonadIO m, HasHttpManager env, MonadReader env m) => Token -> m (Either TelegramBadResponse (TG.Response Bool))
deleteWebhookRequest = telegramGetRequest "deleteWebhook" []

getWebhookInfoRequest :: (MonadThrow m, MonadIO m, HasHttpManager env, MonadReader env m) => Token -> m (Either TelegramBadResponse (TG.Response TG.WebhookInfo))
getWebhookInfoRequest = telegramGetRequest "getWebhookInfo" []


---------------------------------
-- MULTIPART/FORMDATA VERSIONS --
---------------------------------

sendPhotoFileRequest :: (MonadThrow m, MonadIO m, HasHttpManager env, MonadReader env m) => TelegramRequest TG.SendPhotoRequest m TG.Message
sendPhotoFileRequest = telegramPostFileRequest "sendPhoto" []

sendAudioFileRequest :: (MonadThrow m, MonadIO m, HasHttpManager env, MonadReader env m) => TelegramRequest TG.SendAudioRequest m TG.Message
sendAudioFileRequest = telegramPostFileRequest "sendAudio" []

sendDocumentFileRequest :: (MonadThrow m, MonadIO m, HasHttpManager env, MonadReader env m) => TelegramRequest TG.SendDocumentRequest m TG.Message
sendDocumentFileRequest = telegramPostFileRequest "sendDocument" []

sendStickerFileRequest :: (MonadThrow m, MonadIO m, HasHttpManager env, MonadReader env m) => TelegramRequest TG.SendStickerRequest m TG.Message
sendStickerFileRequest = telegramPostFileRequest "sendSticker" []

sendVideoFileRequest :: (MonadThrow m, MonadIO m, HasHttpManager env, MonadReader env m) => TelegramRequest TG.SendVideoRequest m TG.Message
sendVideoFileRequest = telegramPostFileRequest "sendVideo" []

sendVoiceFileRequest :: (MonadThrow m, MonadIO m, HasHttpManager env, MonadReader env m) => TelegramRequest TG.SendVoiceRequest m TG.Message
sendVoiceFileRequest = telegramPostFileRequest "sendVoice" []


setWebhookFileRequest :: (MonadThrow m, MonadIO m, HasHttpManager env, MonadReader env m) => TelegramRequest TG.WebhookRequest m Bool
setWebhookFileRequest = telegramPostFileRequest "setWebhook" []


----------------------
-- HELPER FUNCTIONS --
----------------------

-- | This function checks if the string in the to-be-sent-file argument is a path or url
-- and will send the file itself, or download the file and send it then.
-- It will also keep anything else intact but I don't know if Telegram will accept it if
-- there's no actual file in this `multipart/formdata`

-- | Please use the `telegramPostJSONRequest` if you want to send a url or a file_id
telegramPostFileRequest :: (MonadThrow m, MonadIO m, HasHttpManager env, MonadReader env m, ToJSON a, FromJSON b) =>
                       String -> [(ByteString,Maybe ByteString)] -> a -> Token -> m (Either TelegramBadResponse (TG.Response b))
telegramPostFileRequest url querystring a token = do
  request <- goPR token url
  case toJSON a of
    Object o -> do
      partlist <- mapM valueToPart $ HM.toList o
      goHTTP =<< formDataBody partlist request
    _ -> liftIO $ throwIO NotMultipartable
 where
  valueToPart :: (MonadThrow m, MonadIO m, HasHttpManager env, MonadReader env m) => (Text,Value) -> m Part
  valueToPart (t,Number n)   | Right i <- floatingOrInteger n = return . partBS t . TE.encodeUtf8 $ tshow i
                             | otherwise = return . partBS t . TE.encodeUtf8 $ tshow n
  valueToPart (t,Bool True)  = return $ partBS t "true"
  valueToPart (t,Bool False) = return $ partBS t "false"
  valueToPart (t,String s)   | t `elem` inputFileFields
                             , T.take 1 s == "/"
                                = return $ fileFromPath t $ T.unpack s
                             | t `elem` inputFileFields
                             , (T.take 7 s == "http://" || T.take 8 s == "https://")
                                = return . fileFromUrl t (T.unpack s) =<< asks getHttpManager
                             | otherwise = return $ partBS t $ TE.encodeUtf8 s
  valueToPart (t,v)          = return $ partLBS t $ encode v
  fileFromPath name path = let part = (partFileSource name path)
                           in part { partFilename = getNameFromPath <$> partFilename part }
  fileFromUrl name url mngr = partFileRequestBodyM name (getNameFromPath url) $ do
      fmap (RequestBodyLBS . responseBody) . flip CLIENT.httpLbs mngr =<< parseRequest url
  getNameFromPath s = if mlastpart == []
                        then s
                        else getNameFromPath $ drop 1 mlastpart
    where mlastpart = dropWhile (/= '/') s

inputFileFields :: [Text]
inputFileFields = ["audio","photo","document","sticker","video","voice","certificate"]

data TelegramException = NotMultipartable deriving (Show, Typeable)
instance Exception TelegramException


telegramPostJSONRequest :: (MonadThrow m, MonadIO m, HasHttpManager env, MonadReader env m, ToJSON a, FromJSON b) =>
                       String -> [(ByteString,Maybe ByteString)] -> a -> Token -> m (Either TelegramBadResponse (TG.Response b))
telegramPostJSONRequest url querystring a token = do
  req' <- goPR token url
  let req = req' { method = "POST"
                 , requestBody = RequestBodyLBS $ encode a
                 , requestHeaders = [(hContentType,"application/json")]
                 }
      request = flip setQueryString req querystring
  goHTTP request


telegramGetRequest :: (MonadThrow m, MonadIO m, HasHttpManager env, MonadReader env m, FromJSON a) =>
                       String -> [(ByteString,Maybe ByteString)] -> Token -> m (Either TelegramBadResponse (TG.Response a))
telegramGetRequest url querystring token = do
  req <- goPR token url
  let request = flip setQueryString req querystring
  goHTTP request 


goPR :: (MonadThrow m, MonadIO m, HasHttpManager env, MonadReader env m) => Token -> String -> m Request
goPR token url = parseRequest $ "https://api.telegram.org/bot" <> T.unpack token <> "/" <> url

goHTTP :: (MonadThrow m, MonadIO m, HasHttpManager env, MonadReader env m, FromJSON b) => Request -> m (Either TelegramBadResponse (TG.Response b))
goHTTP req = do
    res <- httpLbs req
    let response = responseBody res
    case eitherDecode' response of
        Right res2    -> return $ Right res2
        Left firsterr -> return $ Left $ TelegramBadResponse (T.pack firsterr) $ LB.toStrict response
