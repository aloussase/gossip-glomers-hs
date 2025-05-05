module Main where

import           Data.Function          ((&))
import           Maelstrom

type State = Int

uniqueIdsHandler :: HandlerFunc State
uniqueIdsHandler msg = do
  node <- getNode

  current_count <- modifyState (+1)

  let id = nodeIdAsNumber node + 3 * current_count

  reply
    $ set_message_type "generate_ok"
    $ add_kvs [("id", id)]
    $ msg

main :: IO ()
main = runNode 1 $ mkNode & addHandler ("generate", uniqueIdsHandler)

-- Executable is at:
-- /Users/aloussase/Code/DistSys/gossip-glommers-hs/dist-newstyle/build/x86_64-osx/ghc-9.4.8/gossip-glommers-hs-0.1.0.0/x/unique-ids/build/unique-ids/unique-ids
