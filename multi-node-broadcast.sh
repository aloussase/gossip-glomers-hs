#!/bin/bash

mael='../maelstrom/maelstrom'
exe='/Users/aloussase/Code/DistSys/gossip-glommers-hs/dist-newstyle/build/x86_64-osx/ghc-9.4.8/gossip-glommers-hs-0.1.0.0/x/multi-node-broadcast/build/multi-node-broadcast/multi-node-broadcast'

"${mael}" test -w broadcast --bin "${exe}" --node-count 5 --time-limit 20 --rate 10
