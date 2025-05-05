#!/bin/bash

../maelstrom/maelstrom test -w unique-ids \
    --bin /Users/aloussase/Code/DistSys/gossip-glommers-hs/dist-newstyle/build/x86_64-osx/ghc-9.4.8/gossip-glommers-hs-0.1.0.0/x/unique-ids/build/unique-ids/unique-ids \
    --time-limit 30 \
    --rate 1000 \
    --node-count 3 \
    --availability total \
    --nemesis partition
