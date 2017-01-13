{-# LANGUAGE RecordWildCards, PatternGuards #-}

module Network.Telegram.Bot.Requests
  ( TelegramRequest
  , TelegramException (..)

  , getMeRequest
  , sendChatActionRequest
  , sendMessageRequest
  , forwardMessageRequest
  , answerCallbackQueryRequest
  , sendPhotoRequest
  , sendAudioRequest
  , sendDocumentRequest
  , sendStickerRequest
  , sendVideoRequest
  , sendVoiceRequest
  , sendLocationRequest
  , sendVenueRequest
  , sendContactRequest
  , getUserProfilePhotosRequest
  , getFileRequest
  , downloadFileRequestToDisk
  , kickChatMemberRequest
  , leaveChatRequest
  , unbanChatMemberRequest
  , getChatRequest
  , getChatAdministratorsRequest
  , getChatMembersCountRequest
  , getChatMemberRequest
  , editMessageTextRequest
  , editMessageCaptionRequest
  , editMessageReplyMarkupRequest
  , answerInlineQueryRequest
  , sendGameRequest
  , setGameScoreRequest
  , getGameHighScoresRequest
  , getUpdatesRequest
  , setWebhookRequest
  , deleteWebhookRequest

  , getWebhookInfoRequest
  , sendPhotoFileRequest
  , sendAudioFileRequest
  , sendDocumentFileRequest
  , sendStickerFileRequest
  , sendVideoFileRequest
  , sendVoiceFileRequest
  , setWebhookFileRequest

  , inputFileFields
  ) where


import           Control.Exception
import           Control.Monad.Catch        (MonadThrow)
import           Control.Monad.IO.Class     (MonadIO, liftIO)

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
import           Data.Typeable

import           Network.HTTP.Client        hiding (httpLbs)
import           Network.HTTP.Client.MultipartFormData
import           Network.HTTP.Conduit
import           Network.HTTP.Types         (hContentType)

import qualified Web.Telegram.Bot           as TG

import           Network.Telegram.Bot.Types


type TelegramRequest a m b = a -> Token -> Manager -> m (Either TelegramBadResponse (TG.Response b))

tshow :: Show a => a -> Text
tshow = T.pack . show

----------------------------
-- SEND AND GET FUNCTIONS --
----------------------------

getMeRequest :: (MonadIO m, MonadThrow m) => Token -> Manager -> m (Either TelegramBadResponse (TG.Response TG.User))
getMeRequest = telegramGetRequest "getMe" []

sendChatActionRequest :: (MonadIO m, MonadThrow m) => TelegramRequest TG.SendChatActionRequest m TG.Message
sendChatActionRequest = telegramPostJSONRequest "sendChatAction" []

sendMessageRequest :: (MonadIO m, MonadThrow m) => TelegramRequest TG.SendMessageRequest m TG.Message
sendMessageRequest = telegramPostJSONRequest "sendMessage" []

forwardMessageRequest :: (MonadIO m, MonadThrow m) => TelegramRequest TG.ForwardMessageRequest m TG.Message
forwardMessageRequest = telegramPostJSONRequest "forwardMessage" []

answerCallbackQueryRequest :: (MonadIO m, MonadThrow m) => TelegramRequest TG.AnswerCallbackQueryRequest m Bool
answerCallbackQueryRequest = telegramPostJSONRequest "answerCallbackQuery" []


sendPhotoRequest :: (MonadIO m, MonadThrow m) => TelegramRequest TG.SendPhotoRequest m TG.Message
sendPhotoRequest = telegramPostJSONRequest "sendPhoto" []

sendAudioRequest :: (MonadIO m, MonadThrow m) => TelegramRequest TG.SendAudioRequest m TG.Message
sendAudioRequest = telegramPostJSONRequest "sendAudio" []

sendDocumentRequest :: (MonadIO m, MonadThrow m) => TelegramRequest TG.SendDocumentRequest m TG.Message
sendDocumentRequest = telegramPostJSONRequest "sendDocument" []

sendStickerRequest :: (MonadIO m, MonadThrow m) => TelegramRequest TG.SendStickerRequest m TG.Message
sendStickerRequest = telegramPostJSONRequest "sendSticker" []

sendVideoRequest :: (MonadIO m, MonadThrow m) => TelegramRequest TG.SendVideoRequest m TG.Message
sendVideoRequest = telegramPostJSONRequest "sendVideo" []

sendVoiceRequest :: (MonadIO m, MonadThrow m) => TelegramRequest TG.SendVoiceRequest m TG.Message
sendVoiceRequest = telegramPostJSONRequest "sendVoice" []

sendLocationRequest :: (MonadIO m, MonadThrow m) => TelegramRequest TG.SendLocationRequest m TG.Message
sendLocationRequest = telegramPostJSONRequest "sendLocation" []

sendVenueRequest :: (MonadIO m, MonadThrow m) => TelegramRequest TG.SendVenueRequest m TG.Message
sendVenueRequest = telegramPostJSONRequest "sendVenue" []

sendContactRequest :: (MonadIO m, MonadThrow m) => TelegramRequest TG.SendContactRequest m TG.Message
sendContactRequest = telegramPostJSONRequest "sendContact" []


getUserProfilePhotosRequest :: (MonadIO m, MonadThrow m) =>
                            TelegramRequest TG.UserProfilePhotosRequest m TG.UserProfilePhotos
getUserProfilePhotosRequest = telegramPostJSONRequest "getUserProfilePhotos" []

getFileRequest :: (MonadIO m, MonadThrow m) => TelegramRequest TG.FileRequest m TG.File
getFileRequest = telegramPostJSONRequest "getFile" []

-- | Returns Nothing on success and a tuple with the error message and maybe
--   Response Parameters to maybe automatically handle certain errors on failure
downloadFileRequestToDisk :: (MonadIO m, MonadThrow m) =>
                          FilePath -> TG.FileRequest -> Token -> Manager -> m (Maybe (Text,Maybe TG.ResponseParameters))
downloadFileRequestToDisk saveTo req token mngr = do
  tgFile <- getFileRequest req token mngr
  case tgFile of
    Right (TG.OKResponse (TG.File _ _ (Just urlpath)) _) -> do
      request <- parseRequest $ "https://api.telegram.org/file/bot" <> T.unpack token <> "/" <> T.unpack urlpath
      liftIO $ withResponse request mngr go
    Right (TG.OKResponse (TG.File _ _ Nothing) mdesc) -> return $ Just $
      ("downloadFileRequestToDisk: NO URL GIVEN" <> maybe "" (" - WITH DESC: " <>) mdesc,Nothing)
    Right (TG.ErrorResponse desc mcode mrp) -> return $ Just $
      ("downloadFileRequestToDisk: ERROR: " <> desc <> maybe "" (mappend " - CODE: " . tshow) mcode,mrp)
    Left (TelegramBadResponse parsefail _) -> return $ Just $ ("downloadFileRequestToDisk: BAD RESPONSE: " <> parsefail,Nothing)
 where
  go res = do
    b <- brRead bodyReader
    case b of
      "" -> return $ Just ("downloadFileRequestToDisk: EMPTY FILE",Nothing)
      _  -> B.writeFile saveTo b >> loop bodyReader
    where bodyReader = responseBody res
          loop breader = do
            br <- brRead breader
            case br of
              "" -> return $ Nothing
              _  -> B.appendFile saveTo br >> loop breader

-------------------
-- CHAT REQUESTS --
-------------------

kickChatMemberRequest :: (MonadIO m, MonadThrow m) => TelegramRequest TG.KickChatMemberRequest m Bool
kickChatMemberRequest = telegramPostJSONRequest "kickChatMember" []

leaveChatRequest ::(MonadIO m, MonadThrow m) => TelegramRequest TG.LeaveChatRequest m Bool
leaveChatRequest = telegramPostJSONRequest "leaveChat" []

unbanChatMemberRequest :: (MonadIO m, MonadThrow m) => TelegramRequest TG.UnbanChatMemberRequest m Bool
unbanChatMemberRequest = telegramPostJSONRequest "unbanChatMember" []

getChatRequest :: (MonadIO m, MonadThrow m) => TelegramRequest TG.GetChatRequest m TG.Chat
getChatRequest = telegramPostJSONRequest "getChat" []

getChatAdministratorsRequest :: (MonadIO m, MonadThrow m) =>
                            TelegramRequest TG.GetChatAdministratorsRequest m [TG.ChatMember]
getChatAdministratorsRequest = telegramPostJSONRequest "getChatAdministrators" []

getChatMembersCountRequest :: (MonadIO m, MonadThrow m) => TelegramRequest TG.GetChatMembersCountRequest m Int
getChatMembersCountRequest = telegramPostJSONRequest "getChatMembersCount" []

getChatMemberRequest :: (MonadIO m, MonadThrow m) => TelegramRequest TG.GetChatMemberRequest m TG.ChatMember
getChatMemberRequest = telegramPostJSONRequest "getChatMember" []


---------------------
-- UPDATE REQUESTS --
---------------------

editMessageTextRequest :: (MonadIO m, MonadThrow m) => TelegramRequest TG.EditMessageTextRequest m Bool
editMessageTextRequest = telegramPostJSONRequest "editMessageText" []

editMessageCaptionRequest :: (MonadIO m, MonadThrow m) => TelegramRequest TG.EditMessageCaptionRequest m Bool
editMessageCaptionRequest = telegramPostJSONRequest "editMessageCaption" []

editMessageReplyMarkupRequest :: (MonadIO m, MonadThrow m) => TelegramRequest TG.EditMessageReplyMarkupRequest m Bool
editMessageReplyMarkupRequest = telegramPostJSONRequest "editMessageReplyMarkupRequest" []


---------------------
-- INLINE REQUESTS --
---------------------

answerInlineQueryRequest :: (MonadIO m, MonadThrow m) => TelegramRequest TG.EditMessageTextRequest m (Either Bool TG.Message)
answerInlineQueryRequest = telegramPostJSONRequest "answerInlineQuery" []


-------------------
-- GAME REQUESTS --
-------------------

sendGameRequest :: (MonadIO m, MonadThrow m) => TelegramRequest TG.SendGameRequest m TG.Message
sendGameRequest = telegramPostJSONRequest "sendGame" []

setGameScoreRequest :: (MonadIO m, MonadThrow m) => TelegramRequest TG.SetGameScoreRequest m (Either Bool TG.Message)
setGameScoreRequest = telegramPostJSONRequest "setGameScore" []

getGameHighScoresRequest :: (MonadIO m, MonadThrow m) => TelegramRequest TG.GetGameHighScoresRequest m [TG.GameHighScore]
getGameHighScoresRequest = telegramPostJSONRequest "getGameHighScores" []


-------------------
-- UTIL REQUESTS --
-------------------

getUpdatesRequest :: (MonadIO m, MonadThrow m) => TelegramRequest TG.UpdatesRequest m [TG.Update]
getUpdatesRequest = telegramPostJSONRequest "getUpdates" []

-- | DON'T USE THIS IF YOU WANT TO SEND A CERTIFICATE, USE setWebhookFileRequest IN THAT CASE
setWebhookRequest :: (MonadIO m, MonadThrow m) => TelegramRequest TG.WebhookRequest m Bool
setWebhookRequest = telegramPostJSONRequest "setWebhook" []

deleteWebhookRequest :: (MonadIO m, MonadThrow m) => Token -> Manager -> m (Either TelegramBadResponse (TG.Response Bool))
deleteWebhookRequest = telegramGetRequest "deleteWebhook" []

getWebhookInfoRequest :: (MonadIO m, MonadThrow m) => Token -> Manager -> m (Either TelegramBadResponse (TG.Response TG.WebhookInfo))
getWebhookInfoRequest = telegramGetRequest "getWebhookInfo" []


---------------------------------
-- MULTIPART/FORMDATA VERSIONS --
---------------------------------

sendPhotoFileRequest :: (MonadIO m, MonadThrow m) => TelegramRequest TG.SendPhotoRequest m TG.Message
sendPhotoFileRequest = telegramPostFileRequest "sendPhoto"

sendAudioFileRequest :: (MonadIO m, MonadThrow m) => TelegramRequest TG.SendAudioRequest m TG.Message
sendAudioFileRequest = telegramPostFileRequest "sendAudio"

sendDocumentFileRequest :: (MonadIO m, MonadThrow m) => TelegramRequest TG.SendDocumentRequest m TG.Message
sendDocumentFileRequest = telegramPostFileRequest "sendDocument"

sendStickerFileRequest :: (MonadIO m, MonadThrow m) => TelegramRequest TG.SendStickerRequest m TG.Message
sendStickerFileRequest = telegramPostFileRequest "sendSticker"

sendVideoFileRequest :: (MonadIO m, MonadThrow m) => TelegramRequest TG.SendVideoRequest m TG.Message
sendVideoFileRequest = telegramPostFileRequest "sendVideo"

sendVoiceFileRequest :: (MonadIO m, MonadThrow m) => TelegramRequest TG.SendVoiceRequest m TG.Message
sendVoiceFileRequest = telegramPostFileRequest "sendVoice"


setWebhookFileRequest :: (MonadIO m, MonadThrow m) => TelegramRequest TG.WebhookRequest m Bool
setWebhookFileRequest = telegramPostFileRequest "setWebhook"


----------------------
-- HELPER FUNCTIONS --
----------------------

-- | This function checks if the string in the to-be-sent-file argument is a path or url
-- and will send the file itself, or download the file and send it then.
-- It will also keep anything else intact but I don't know if Telegram will accept it if
-- there's no actual file in this `multipart/formdata`

-- | Please use the `telegramPostJSONRequest` if you want to send a url or a file_id
telegramPostFileRequest :: (MonadIO m, MonadThrow m, ToJSON a, FromJSON b) =>
                       String -> a -> Token -> Manager -> m (Either TelegramBadResponse (TG.Response b))
telegramPostFileRequest url a token mngr = do
  request <- goPR token url
  case toJSON a of
    Object o -> do
      partlist <- mapM valueToPart $ HM.toList o
      flip goHTTP mngr =<< formDataBody partlist request
    _ -> liftIO $ throwIO NotMultipartable
 where
  valueToPart :: (MonadIO m, MonadThrow m) => (Text,Value) -> m Part
  valueToPart (t,Number n)   | Right i <- (floatingOrInteger n :: Either Double Integer) = return . partBS t . TE.encodeUtf8 $ tshow i
                             | otherwise = return . partBS t . TE.encodeUtf8 $ tshow n
  valueToPart (t,Bool True)  = return $ partBS t "true"
  valueToPart (t,Bool False) = return $ partBS t "false"
  valueToPart (t,String s)   | t `elem` inputFileFields
                             , T.take 1 s == "/"
                                = return $ fileFromPath t $ T.unpack s
                             | t `elem` inputFileFields
                             , (T.take 7 s == "http://" || T.take 8 s == "https://")
                                = return $ fileFromUrl t $ T.unpack s
                             | otherwise = return $ partBS t $ TE.encodeUtf8 s
  valueToPart (t,v)          = return $ partLBS t $ encode v
  fileFromPath name path     = part { partFilename = getNameFromPath <$> partFilename part }
                         where part = (partFileSource name path)
  fileFromUrl name urlpath   = partFileRequestBodyM name (getNameFromPath urlpath) $ do
    fmap (RequestBodyLBS . responseBody) . flip httpLbs mngr =<< parseRequest urlpath
  getNameFromPath s = if mlastpart == []
                        then s
                        else getNameFromPath $ drop 1 mlastpart
    where mlastpart = dropWhile (/= '/') s

inputFileFields :: [Text]
inputFileFields = ["audio","photo","document","sticker","video","voice","certificate"]

data TelegramException = NotMultipartable deriving (Show, Typeable)
instance Exception TelegramException


telegramPostJSONRequest :: (MonadIO m, MonadThrow m, ToJSON a, FromJSON b) =>
                       String -> [(ByteString,Maybe ByteString)] -> a -> Token -> Manager -> m (Either TelegramBadResponse (TG.Response b))
telegramPostJSONRequest url querystring a token mngr = do
  req' <- goPR token url
  let req = req' { method = "POST"
                 , requestBody = RequestBodyLBS $ encode a
                 , requestHeaders = [(hContentType,"application/json")]
                 }
      request = flip setQueryString req querystring
  goHTTP request mngr


telegramGetRequest :: (MonadIO m, MonadThrow m, FromJSON a) =>
                       String -> [(ByteString,Maybe ByteString)] -> Token -> Manager -> m (Either TelegramBadResponse (TG.Response a))
telegramGetRequest url querystring token mngr = do
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
