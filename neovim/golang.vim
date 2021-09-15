" here go all the golang related settings

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
let g:go_def_mapping_enabled = 0

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
