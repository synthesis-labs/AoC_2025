echo "Created $DAY"
#!/bin/bash
set -euo pipefail

if [ $# -ne 1 ]; then
	echo "Usage: ./new_day.sh <day_number>"
	exit 1
fi

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
DAY=$(printf "day%02d" "$1")
DAY_DIR="$SCRIPT_DIR/days/$DAY"

mkdir -p "$DAY_DIR"
cd "$DAY_DIR"
cargo init --name "$DAY" --vcs none >/dev/null

cat > Cargo.toml << EOF
[package]
name = "$DAY"
version = "0.1.0"
edition = "2021"

[dependencies]
utils = { path = "../../src/utils" }
anyhow = { workspace = true }
nom = { workspace = true }
rayon = { workspace = true }
EOF

cp "$SCRIPT_DIR/src/day_template.rs" src/main.rs
touch input.txt

echo "Created $DAY"