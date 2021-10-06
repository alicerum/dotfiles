#!/usr/bin/env bash

# directory in which this script is located
SCRIPT_DIR="$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )"

mkdir -p "$HOME/.config/kitty"
pushd "$HOME/.config/kitty" >/dev/null || exit 1

# create symlinks to neovim config fileses
for i in $(find "$SCRIPT_DIR" -name "*.conf" -type f); do
	ln -s "$i" .
done

popd >/dev/null || exit 1
