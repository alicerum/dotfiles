runtime plug.vim
runtime mappings.vim
runtime visuals.vim
runtime golang.vim
runtime rust.vim
runtime haskell.vim
runtime coc.vim
runtime vimspector.vim

lua require'nvim-treesitter.install'.compilers = { "gcc" }

" let NERDTreeShowHidden = 1


" this is the fix for stuck netrw windows
" with vinegar on '-'
let g:netrw_fastbrowse=0

set clipboard+=unnamedplus
set showtabline=2
set number
set nobackup
set nowritebackup
set splitbelow
set cmdheight=2
set updatetime=300
set shortmess+=c
set signcolumn=yes

augroup docker
	autocmd!

	autocmd BufNewFile,BufRead Dockerfile* set syntax=dockerfile
augroup END
