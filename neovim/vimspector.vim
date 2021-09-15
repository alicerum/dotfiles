let g:vimspector_enable_mappings = 'HUMAN'

nnoremap <Leader>dd :call vimspector#Launch()<CR>
nnoremap <Leader>de :call vimspector#Reset()<CR>
nnoremap <Leader>dc :call vimspector#Continue()<CR>

nnoremap <Leader>dT :call vimspector#ClearBreakpoints()<CR>

nmap <Leader>di <Plug>VimspectorBalloonEval
xmap <Leader>di <Plug>VimspectorBalloonEval

nnoremap <F9> <Plug>VimspectorToggleBreakpoint<CR>
