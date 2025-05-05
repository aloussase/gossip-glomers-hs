module Main where

import Maelstrom

import Data.Function ((&))

data State = MkState
    { sMessages :: [Int]
    }
    deriving Show

handleBroadcast :: HandlerFunc State
handleBroadcast = do
    -- APIS needed
    -- 1. get_value : Make the value be FromJSON
    -- 2. add_kv : Make the value be FromJSON

    return undefined

main :: IO ()
main = runNode (MkState []) $ mkNode & addHandler ("broadcast", handleBroadcast)
