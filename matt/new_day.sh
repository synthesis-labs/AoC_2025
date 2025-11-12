#!/bin/bash
DAY=$(printf "day%02d" $1)
mkdir -p days/$DAY
cd days/$DAY
cargo init --name $DAY

cat > Cargo.toml << EOF
[package]
name = "$DAY"
version = "0.1.0"
edition = "2021"

[dependencies]
utils = { path = "../../utils" }
anyhow = { workspace = true }
EOF

# Copy template main.rs (you'd create a template file first)
cp ../../src/day_template.rs src/main.rs
touch input.txt

echo "Created $DAY"