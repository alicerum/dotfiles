"Plugiiiins
call plug#begin('~/.local/share/nvim/plugged')
	Plug 'preservim/nerdtree'
	Plug 'fatih/vim-go', { 'do': ':GoUpdateBinaries' }

	Plug 'rust-lang/rust.vim'

	Plug 'neoclide/coc.nvim', {'branch': 'release'}

	Plug 'mhinz/vim-signify'

	Plug 'Shougo/neosnippet.vim'
	Plug 'Shougo/neosnippet-snippets'

	Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }

	Plug 'itchyny/lightline.vim'
	Plug 'mengelbrecht/lightline-bufferline'

	Plug 'puremourning/vimspector'

	Plug 'nvim-treesitter/nvim-treesitter', {'do': ':TSUpdate'}

	Plug 'tpope/vim-fugitive'

	Plug 'drewtempelmeyer/palenight.vim'
	Plug 'arcticicestudio/nord-vim'
	Plug 'sonph/onehalf', { 'rtp': 'vim' }
	Plug 'ayu-theme/ayu-vim'
	Plug 'joshdick/onedark.vim'
	Plug 'ntk148v/vim-horizon'
	Plug 'overcache/NeoSolarized'
	Plug 'ryanoasis/vim-devicons'
call plug#end()
