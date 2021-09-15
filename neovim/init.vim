runtime plug.vim
runtime mappings.vim
runtime visuals.vim
runtime golang.vim
runtime coc.vim
runtime vimspector.vim

let NERDTreeShowHidden = 1

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
