call plug#begin('~/.local/share/nvim/plugged')
Plug 'preservim/nerdtree'
Plug 'fatih/vim-go', { 'do': ':GoUpdateBinaries' }

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

set clipboard+=unnamedplus

"set termguicolors     " enable true colors support
" let ayucolor="light"  " for light version of theme
" let ayucolor="mirage" " for mirage version of theme
"let ayucolor="mirage"   " for dark version of theme
colorscheme onedark

" if you don't set this option, this color might not correct

set t_Co=256
" colorscheme onehalfdark
" let g:lightline = { 'colorscheme': 'onehalfdark' }

set background=dark
"colorscheme molokai
"let g:lightline = { 'colorscheme': 'nord' }
"let g:lightline.tabline = {'left': [['buffers']], 'right': [['close']]}

set showtabline=2
let g:lightline = {
      \ 'colorscheme': 'onedark',
      \ 'active': {
      \   'left': [ [ 'mode', 'paste' ], [ 'readonly', 'filename', 'modified' ] ]
      \ },
      \ 'tabline': {
      \   'left': [ ['buffers'] ],
      \   'right': [ ['close'] ]
      \ },
      \ 'component_expand': {
      \   'buffers': 'lightline#bufferline#buffers'
      \ },
      \ 'component_type': {
      \   'buffers': 'tabsel'
      \ }
      \ }

let g:palenight_terminal_italics=1

let mapleader=","
let maplocalleader="."

set number
set nobackup
set nowritebackup

let g:go_gopls_enabled = 0
let g:go_gopls_options = ['-remote=auto']
let g:go_def_mode='gopls'
let g:go_info_mode='gopls'
let g:go_referrers_mode = 'gopls'
let g:go_fmt_command = "goimports"
let g:go_def_mapping_enabled = 0

" Go highlighting
let g:go_highlight_types = 1
let g:go_highlight_fields = 1
let g:go_highlight_functions = 1
let g:go_highlight_function_calls = 1
let g:go_doc_keywordprg_enabled = 0

let NERDTreeShowHidden = 1

" ============ Coc configuration

let g:go_def_mapping_enabled = 0

set cmdheight=2
set updatetime=300
set shortmess+=c
set signcolumn=yes

" GoTo code navigation.
nmap <silent> gd <Plug>(coc-definition)
nmap <silent> gy <Plug>(coc-type-definition)
nmap <silent> gi <Plug>(coc-implementation)
nmap <silent> gr <Plug>(coc-references)

" use k to show documentation in preview window.
nnoremap <silent> K :call <sid>show_documentation()<cr>


function! s:show_documentation()
  if (index(['vim','help'], &filetype) >= 0)
    execute 'h '.expand('<cword>')
  else
    call CocActionAsync('doHover')
  endif
endfunction

" Make <CR> auto-select the first completion item and notify coc.nvim to
" format on enter, <cr> could be remapped by other vim plugin
inoremap <silent><expr> <cr> pumvisible() ? coc#_select_confirm()
                              \: "\<C-g>u\<CR>\<c-r>=coc#on_enter()\<CR>"

" Highlight the symbol and its references when holding the cursor.
autocmd CursorHold * silent call CocActionAsync('highlight')

" Highlight the symbol and its references when holding the cursor.
nmap <leader>rn <Plug>(coc-rename)“ Formatting selected code.
xmap <leader>f <Plug>(coc-format-selected)
nmap <leader>f <Plug>(coc-format-selected)


let g:vimspector_enable_mappings = 'HUMAN'

nnoremap <Leader>dd :call vimspector#Launch()<CR>
nnoremap <Leader>de :call vimspector#Reset()<CR>
nnoremap <Leader>dc :call vimspector#Continue()<CR>

nmap <Leader>di <Plug>VimspectorBalloonEval
xmap <Leader>di <Plug>VimspectorBalloonEval

nnoremap <F9> <Plug>VimspectorToggleBreakpoint<CR>


" ======= Basic hotkeys

nnoremap <leader>n :NERDTreeFocus<CR>
nnoremap <leader>t :NERDTreeToggle<CR>

nnoremap <silent> <leader>ec :e $MYVIMRC<CR>
nnoremap <silent> <leader>sc :source $MYVIMRC<CR>

" go to next buffer
nnoremap <silent> <leader>bn :bn<CR>
nnoremap <silent> <C-l> :bn<CR>

" go to previous buffer
nnoremap <silent> <leader>bp :bp<CR>
" https://github.com/neovim/neovim/issues/2048
nnoremap <silent> <C-h> :bp<CR>

nnoremap <leader>fs :w<CR>
nnoremap <leader>fq :q<CR>
nnoremap <leader>qa :qall<CR>

nnoremap <leader>ff :FZF<CR>

inoremap <C-u> <esc>viwUea
inoremap jk <esc>
inoremap <esc> <nop>

" close buffer
nnoremap <silent> <leader>bd :bd<CR>

" kill buffer
nnoremap <silent> <leader>bk :bd!<CR>

" list buffers
nnoremap <silent> <leader>bl :ls<CR>
" list and select buffer
nnoremap <silent> <leader>bg :ls<CR>:buffer<Space>

" horizontal split with new buffer
nnoremap <silent> <leader>bh :new<CR>

" vertical split with new buffer
nnoremap <silent> <leader>bv :vnew<CR>

nnoremap <silent> <leader>nf :NERDTreeFind<CR>


" Golang mappings
augroup golang_files
	autocmd!

	autocmd FileType go set noexpandtab
	autocmd FileType go set shiftwidth=4
	autocmd FileType go set softtabstop=4
	autocmd FileType go set tabstop=4

	autocmd FileType go,gomod nnoremap <buffer> <localleader>pb :GoBuild<CR>
	autocmd FileType go,gomod nnoremap <buffer> <localleader>pt :GoTest<CR>

	autocmd FileType go nnoremap <buffer> <localleader>ds :GoDecls<cr>
augroup END

" Rust mappings
augroup rust_files
	autocmd!

	autocmd FileType rust set shiftwidth=4
	autocmd FileType rust set softtabstop=4
	autocmd FileType rust set tabstop=4
	autocmd FileType rust set expandtab
augroup END


if (has("nvim"))
  "For Neovim 0.1.3 and 0.1.4 < https://github.com/neovim/neovim/pull/2198 >
  let $NVIM_TUI_ENABLE_TRUE_COLOR=1
endif

"For Neovim > 0.1.5 and Vim > patch 7.4.1799 < https://github.com/vim/vim/commit/61be73bb0f965a895bfb064ea3e55476ac175162 >
"Based on Vim patch 7.4.1770 (`guicolors` option) < https://github.com/vim/vim/commit/8a633e3427b47286869aa4b96f2bfc1fe65b25cd >
" < https://github.com/neovim/neovim/wiki/Following-HEAD#20160511 >
if (has("termguicolors"))
  set termguicolors
endif

lua <<EOF
require'nvim-treesitter.configs'.setup {
  ensure_installed = "maintained", -- one of "all", "maintained" (parsers with maintainers), or a list of languages
  highlight = {
    enable = true,              -- false will disable the whole extension
  },
}
EOF
