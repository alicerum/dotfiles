local opts = {noremap = true, silent = true}
local setmap = function(mode, map, action)
	vim.api.nvim_set_keymap(mode, map, action, opts)
end

setmap('n', "<Leader>db", ":lua require'dap'.toggle_breakpoint()<CR>")
setmap('n', "<Leader>dc", ":lua require'dap'.continue()<CR>")
setmap('n', "<Leader>dso", ":lua require'dap'.step_over()<CR>")
setmap('n', "<Leader>dsi", ":lua require'dap'.step_into()<CR>")
setmap('n', "<Leader>dro", ":lua require'dap'.repl.open()<CR>")
setmap('n', "<Leader>drc", ":lua require'dap'.repl.close()<CR>")
setmap('n', '<leader>di', ":lua require('dap.ui.widgets').hover()<cr>")

setmap('n', '<Leader>rn', ':lua vim.lsp.buf.rename()<cr>')


setmap('n', "gn", ":cnext<CR>")
setmap('n', "gp", ":cprevious<CR>")
setmap('n', "gc", ":cclose<CR>")

setmap('n', '<leader>tt', '<cmd>NvimTreeToggle<cr>')
setmap('n', '<leader>tf', '<cmd>NvimTreeFocus<cr>')
setmap('n', '<leader>tc', '<cmd>NvimTreeCollapse<cr>')
setmap('n', '<leader>tg', '<cmd>NvimTreeFindFile<cr>')

setmap('n', '<leader>ng', '<cmd>Neogit<cr>')

setmap('n', '<Leader>ee', ':Explore<CR>')

setmap('n', '<Leader>ec', ':e $MYVIMRC<CR>')
setmap('n', '<Leader>sc', ':source $MYVIMRC<CR>')

setmap('n', '<C-h>', '<C-w>h')
setmap('n', '<C-l>', '<C-w>l')
setmap('n', '<C-j>', '<C-w>j')
setmap('n', '<C-k>', '<C-w>k')

setmap('n', '<Leader>bl', ':buffers<CR>')

setmap('n', '<Leader>fs', ':w<CR>')
setmap('n', '<Leader>fq', ':q<CR>')
setmap('n', '<Leader>qa', ':qall<CR>')

setmap('n', '<Leader>bd', ':bd<CR>')

setmap("n", "<leader>ff", "<cmd>lua require('telescope.builtin').find_files()<cr>")
setmap("n", "<leader>fg", "<cmd>lua require('telescope.builtin').live_grep()<cr>")
setmap("n", "<leader>fb", "<cmd>lua require('telescope.builtin').buffers()<cr>")
setmap("n", "<leader>fh", "<cmd>lua require('telescope.builtin').help_tags()<cr>")

setmap('i', '<C-u>', '<esc>viwUea')

setmap('n', '<Leader>bh', ':new<CR>')
setmap('n', '<Leader>bv', ':vnew<CR>')

setmap('n', '<Leader>sh', ':split<CR>')
setmap('n', '<Leader>sv', ':vsplit<CR>')

