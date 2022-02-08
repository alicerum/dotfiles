vim.cmd [[
augroup golang_files
	autocmd!

	autocmd FileType go set noexpandtab
	autocmd FileType go set shiftwidth=4
	autocmd FileType go set softtabstop=4
	autocmd FileType go set tabstop=4
augroup END
]]
