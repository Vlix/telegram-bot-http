module Network.Telegram.Bot.Types where


import           Data.Maybe         (Maybe)
import           Data.Text          (Text)
import           Data.ByteString    (ByteString)


type Token = Text

data TelegramBadResponse =
  TelegramBadResponse
    { success_parse_fail :: Text
    , original_response  :: ByteString
    }
