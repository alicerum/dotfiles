#!/usr/bin/env bash

# directory in which this script is located
SCRIPT_DIR="$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )"

rm -rf "$HOME/.config/nvim"
mkdir "$HOME/.config/nvim"
pushd "$HOME/.config/nvim" >/dev/null || exit 1

# create symlinks to neovim config fileses
for i in $(find "$SCRIPT_DIR" -name "*.vim" -type f); do
	ln -s "$i" .
done

#symlink to coc settings
ln -s "$SCRIPT_DIR/coc-settings.json" .
popd >/dev/null || exit 1

# install plug.vim
sh -c "curl -fLo "${XDG_DATA_HOME:-$HOME/.local/share}"/nvim/site/autoload/plug.vim --create-dirs \
       https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim"
