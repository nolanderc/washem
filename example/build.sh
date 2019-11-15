#!/bin/bash

rustc $1.rs --target wasm32-unknown-unknown -C opt-level=z -C debuginfo=0 --crate-type cdylib
wasm-strip $1.wasm
wasm-opt $1.wasm -o $1.wasm

