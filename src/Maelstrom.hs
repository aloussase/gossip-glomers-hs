module Maelstrom
 ( MessageHandler
 , Node
 , runNode
 , reply
 , addHandler
 , HandlerFunc
 , Message
 , message_type
 , set_message_type
 , mkNode
 ) where

import           Control.Concurrent.Async   (wait, withAsync)
import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.ByteString.Lazy.Char8 as BSL8
import           Data.Maybe                 (fromJust)
import           Deriving.Aeson.Stock
import           System.IO

data Message = Message
  { message_src  :: !String
  , message_dest :: !String
  , message_body :: !Value
  }
  deriving stock (Show, Generic)
  deriving (FromJSON, ToJSON) via PrefixedSnake "message_" Message

type HandlerFunc = Message -> IO ()

data MessageHandler = MessageHandler
  { handler_name :: !String
  , handler_func :: HandlerFunc
  }

data Node = Node
  { node_handlers :: [(String, MessageHandler)]
  , node_id       :: !(Maybe String)
  , node_peers    :: ![String]
  }

mkNode :: Node
mkNode = Node
  { node_handlers = []
  , node_id       = Nothing
  , node_peers    = []
  }

mkMessage :: String -> Maybe Message
mkMessage = decode . BSL8.pack

set_in_reply_to :: Int -> Message -> Message
set_in_reply_to id' msg =
  case message_body msg of
    Object o ->
      let Object kv = object ["in_reply_to" .= id']
          o' = Object (kv <> o) in
      msg { message_body = o' }
    _ -> undefined

set_message_type :: String -> Message -> Message
set_message_type typ msg =
  case message_body msg of
    Object o ->
      let Object kv = object ["type" .= typ]
          o' = Object (kv <> o) in
      msg { message_body = o' }
    _ -> undefined

-- | Reply to the message.
reply :: Message -> IO ()
reply msg = do
  let msg' = set_in_reply_to
              (message_id msg)
              (msg { message_src = message_dest msg , message_dest = message_src msg })
  sendMessage msg'

-- | For internal use only.
sendMessage :: Message -> IO ()
sendMessage msg = do
  let payload = object
          [ "src" .= message_src msg
          , "dest" .= message_dest msg
          , "body" .= message_body msg
          ]
      strPayload = encode payload
  BSL8.putStrLn strPayload
  hFlush stdout

runNode :: Node -> IO ()
runNode node = do
  line <- getLine
  if null line
  then pure ()
  else
    case mkMessage line of
      Just msg -> do
        node' <- handleMessage node msg
        runNode node'
      Nothing  -> error $ "Failed to parse message: " <> line

addHandler :: (String, HandlerFunc) -> Node -> Node
addHandler (name, func) node =
  let handler = MessageHandler name func
      handlers = (name, handler) : node_handlers node
  in node { node_handlers = handlers }

message_type :: Message -> String
message_type msg =
  let parser = withObject "Message type parser" $ \o -> do
        typ <- o .: "type"
        return typ
    in fromJust $ parseMaybe parser $ message_body msg

message_id :: Message -> Int
message_id msg =
  let parser = withObject "Message ID parser" $ \o -> do
        id' <- o .: "msg_id"
        return id'
    in fromJust $ parseMaybe parser $ message_body msg

handleMessage :: Node -> Message -> IO Node
handleMessage node msg =
  let handler = lookup (message_type msg) (node_handlers node) in
  case (message_type msg, handler) of
    ("init", _)    -> _init node msg
    (_, Just hdlr) -> withAsync (handler_func hdlr msg) wait >> pure node
    (typ, _)       -> error $ "No handler registered for message type: " <> typ

_init :: Node -> Message -> IO Node
_init node msg = do
  case node_id node of
    Just _  -> error "Node has already been initialized"
    Nothing -> do
      let body = message_body msg
          parser = withObject "Init message parser" $ \o -> do
            id' <- o .: "node_id"
            ids <- o .: "node_ids"
            return (id', ids)
        in case parseMaybe parser body of
          Just (id', ids) -> do
            let msg' = Message
                  { message_src = message_dest msg
                  , message_dest = message_src msg
                  , message_body = object
                        [ "in_reply_to" .= message_id msg
                        , "type" .= ("init_ok" :: String)
                        ]
                  }
            sendMessage msg' >> pure (node { node_id = Just id', node_peers = ids })
          _               -> error "Invalid init message"


