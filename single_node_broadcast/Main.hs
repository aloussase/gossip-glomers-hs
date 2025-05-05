module Main where

import Maelstrom

import Data.Function ((&))

data State = MkState
    { sMessages :: [Int]
    }
    deriving Show

handleBroadcast :: HandlerFunc State
handleBroadcast m = do
    let n = get_value "message" m

    _ <- modifyState $ \s -> MkState (n : sMessages s)

    let r = delete_key "message" $ set_message_type "broadcast_ok" m

    reply r

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
         & addHandler ("read", handleRead)
         & addHandler ("topology", handleTopology)
         )
