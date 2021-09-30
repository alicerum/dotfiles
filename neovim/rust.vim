" some rust related settings

augroup rust_files
	autocmd!

	autocmd FileType rust set shiftwidth=4
	autocmd FileType rust set softtabstop=4
	autocmd FileType rust set tabstop=4
	autocmd FileType rust set expandtab

	autocmd FileType rust nnoremap <buffer> <localleader>pb :Cargo build<CR>
	autocmd FileType rust nnoremap <buffer> <localleader>pr :Cargo run<CR>
augroup END
