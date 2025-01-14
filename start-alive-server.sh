#!/bin/bash

# Set the path to the alive-lsp.asd file
ALIVE_LSP_PATH="/Users/sample/quicklisp/local-projects/alive-lsp/alive-lsp.asd"

# Set the full path to sbcl
SBCL_PATH="/opt/homebrew/bin/sbcl"

# エラーチェック
set -e

# alive-lsp の起動コマンド
$SBCL_PATH --load $ALIVE_LSP_PATH --eval "(ql:quickload \"alive-lsp\")" --eval "(alive/server::start :port 8006)"
