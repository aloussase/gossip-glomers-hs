module Maelstrom
 ( MessageHandler
 , Node (node_id, node_peers)
 , runNode
 , reply
 , addHandler
 , HandlerFunc
 , Message
 , message_type
 , set_message_type
 , mkNode
 , getNode
 , getState
 , modifyState
 , nodeIdAsNumber
 , add_kv
 , add_kvs
 , get_value
 ) where


import Data.List (foldl')
import           Control.Concurrent.Async    (wait, withAsync)
import           Control.Concurrent.STM      (atomically)
import           Control.Concurrent.STM.TVar
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.ByteString.Lazy.Char8  as BSL8
import           Data.Maybe                  (fromJust)
import           Data.String                 (IsString (..))
import           Deriving.Aeson.Stock
import           System.IO

data Message = Message
  { message_src  :: !String
  , message_dest :: !String
  , message_body :: !Value
  }
  deriving stock (Show, Generic)
  deriving (FromJSON, ToJSON) via PrefixedSnake "message_" Message

data State a = State
  { state_node :: Node a
  , state_user :: TVar a
  }

type Context a = ReaderT (State a) IO

getNode :: Context a (Node a)
getNode = asks state_node

nodeIdAsNumber :: Node a -> Int
nodeIdAsNumber node =
  case node_id node of
    Just ('n' : n) -> read n
    _              -> error "Invalid node ID"

getState :: Context a a
getState = liftIO . readTVarIO =<< asks state_user

modifyState :: (a -> a) -> Context a a
modifyState f = do
  state <- asks state_user
  old_state <- liftIO $ readTVarIO state
  liftIO $ atomically $ stateTVar state (\s -> (old_state, f s))

type HandlerFunc a = Message -> Context a ()

data MessageHandler a = MessageHandler
  { handler_name :: !String
  , handler_func :: HandlerFunc a
  }

data Node a = Node
  { node_handlers :: [(String, MessageHandler a)]
  , node_id       :: !(Maybe String)
  , node_peers    :: ![String]
  }

mkNode :: Node a
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

-- | Add a single key/value pair to the message.
add_kv :: ToJSON value => String -> value -> Message -> Message
add_kv k v m =
    case message_body m of
        Object o ->
            let Object new_kvs = object [fromString k .= toJSON v]
                o' = Object (new_kvs <> o) in
            m { message_body = o' }
        _ -> undefined

-- | Add multiple key/value pairs to the Message.
add_kvs :: ToJSON value => [(String, value)] -> Message -> Message
add_kvs kvs m = foldl' (\m' (k, v) -> add_kv k v m') m kvs

-- | Get a value from the message payload.
get_value :: FromJSON value => String -> Message -> value
get_value k m =
    let parser = withObject "Extract user value" $ \o -> do
                    v <- o .: fromString k
                    case fromJSON v of
                        Error err -> error err
                        Success res -> return res
    in fromJust $ parseMaybe parser $ message_body m

set_message_type :: String -> Message -> Message
set_message_type typ msg =
  case message_body msg of
    Object o ->
      let Object kv = object ["type" .= typ]
          o' = Object (kv <> o) in
      msg { message_body = o' }
    _ -> undefined

-- | Reply to the message.
reply :: MonadIO m => Message -> m ()
reply msg = do
  let msg' = set_in_reply_to
              (message_id msg)
              (msg { message_src = message_dest msg , message_dest = message_src msg })
  liftIO $ sendMessage msg'

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

runNode :: a -> Node a -> IO ()
runNode state node = do
  line <- getLine
  state' <- newTVarIO state

  if null line
  then pure ()
  else
    case mkMessage line of
      Just msg -> do
        node' <- handleMessage state' node msg
        new_state <- readTVarIO state'
        runNode new_state node'
      Nothing  -> error $ "Failed to parse message: " <> line

addHandler :: (String, HandlerFunc a) -> Node a -> Node a
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

handleMessage :: TVar a -> Node a -> Message -> IO (Node a)
handleMessage state node msg =
  let handler = lookup (message_type msg) (node_handlers node)
      usr_state = State node state in
  case (message_type msg, handler) of
    ("init", _)    -> _init node msg
    (_, Just hdlr) -> withAsync (runReaderT (handler_func hdlr msg) usr_state) wait >> pure node
    (typ, _)       -> error $ "No handler registered for message type: " <> typ

_init :: Node a -> Message -> IO (Node a)
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
