set -e # Exit early if any commands fail

(
  cd "$(dirname "$0")" # Ensure compile steps are run within the repository directory
  cargo build --release --target-dir=/tmp/lox-interpreter-build --manifest-path Cargo.toml
)

exec /tmp/lox-interpreter-build/release/lox-interpreter "$@"
