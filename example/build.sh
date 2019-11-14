#!/bin/bash

rustc fib.rs --target wasm32-unknown-unknown -C opt-level=z -C debuginfo=0 --crate-type cdylib
wasm-strip fib.wasm
wasm-opt fib.wasm -o fib.wasm

