#!/bin/bash
cd "$(dirname "$0")"

set -ex
export GOPATH="$PWD"
ragel -sZ src/gopkg.thequux.com/ga144/assembler/parser.rl
go build  ...ga144asm
