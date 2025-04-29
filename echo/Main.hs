module Main where

import           Data.Function ((&))
import           Maelstrom


echoHandler :: HandlerFunc ()
echoHandler = reply . set_message_type "echo_ok"

main :: IO ()
main = runNode () $ mkNode & addHandler ("echo", echoHandler)


