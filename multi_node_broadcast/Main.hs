module Main where

import           Maelstrom

import           Control.Monad          (forM_, when)
import           Control.Monad.IO.Class (liftIO)
import           Data.Function          ((&))

data State = MkState
    { sMessages :: [Int]
    }
    deriving Show

handleBroadcast :: HandlerFunc State
handleBroadcast m = do
    let n = get_value "message" m

    s <- getState

    when (not $ n `elem` sMessages s) $ do
      _ <- modifyState $ \s' -> MkState (n : sMessages s')
      node <- getNode

      forM_ (node_peers node) $ \peer -> do
        -- TODO: Make this work on the app monad.
        liftIO $ sendTo peer node m

    reply $ delete_key "message" $ set_message_type "broadcast_ok" m

handleRead :: HandlerFunc State
handleRead m = do
    s <- getState

    let m' = set_message_type "read_ok" $ add_kv "messages" (sMessages s) $ m

    reply m'


handleTopology :: HandlerFunc State
handleTopology = reply . set_message_type "topology_ok" . delete_key "topology"

main :: IO ()
main = runNode (MkState [])
         ( mkNode
         & addHandler ("broadcast", handleBroadcast)
         & addHandler ("broadcast_ok", \_ -> return ())
         & addHandler ("read", handleRead)
         & addHandler ("topology", handleTopology)
         )
