#!/bin/bash

mael='../maelstrom/maelstrom'
exe='/Users/aloussase/Code/DistSys/gossip-glommers-hs/dist-newstyle/build/x86_64-osx/ghc-9.4.8/gossip-glommers-hs-0.1.0.0/x/single-node-broadcast/build/single-node-broadcast/single-node-broadcast'

"${mael}" test -w broadcast --bin "${exe}" --node-count 1 --time-limit 20 --rate 10
