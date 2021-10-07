#!/usr/bin/env bash

# directory in which this script is located
SCRIPT_DIR="$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )"

ln -sf "$HOME/.zshrc" "$SCRIPT_DIR/zsh/.zshrc"

# shellcheck source=./neovim/install.sh
source "$SCRIPT_DIR"/neovim/install.sh
# shellcheck source=./kitty/install.sh
source "$SCRIPT_DIR"/kitty/install.sh

