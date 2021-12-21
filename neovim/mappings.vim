" All the general mappings go here

let mapleader=","
let maplocalleader="."

" turning off NERDTree for now
"nnoremap <leader>t :NERDTreeToggle<CR>
"nnoremap <silent> <leader>nf :NERDTreeFind<CR>

nnoremap <leader>ee :Explore<CR>

nnoremap <silent> <leader>ec :e $MYVIMRC<CR>
nnoremap <silent> <leader>sc :source $MYVIMRC<CR>

" go to next buffer
nnoremap <silent> <leader>bn :bn<CR>
nnoremap <silent> <C-l> :bn<CR>

nnoremap <silent> s l
nnoremap <silent> l s

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

