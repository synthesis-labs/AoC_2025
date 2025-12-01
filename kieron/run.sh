#!/bin/bash
if [ -z "$1" ]; then
    echo "Usage: ./run.sh <day_number>"
    exit 1
fi

DAY=$(printf "day%02d" $1)
cargo run --package $DAY