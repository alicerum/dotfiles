local opts = {noremap = true, silent = true}
local setmap = function(mode, map, action)
	vim.api.nvim_set_keymap(mode, map, action, opts)
end

setmap('n', '<Leader>ee', ':Explore<CR>')

setmap('n', '<Leader>ec', ':e $MYVIMRC<CR>')
setmap('n', '<Leader>sc', ':source $MYVIMRC<CR>')

-- go to next buffer
setmap('n', '<C-l>', ':bn<CR>')
-- go to previous buffer
setmap('n', '<C-h>', ':bp<CR>')

setmap('n', '<Leader>bl', ':buffers<CR>')

setmap('n', '<Leader>fs', ':w<CR>')
setmap('n', '<Leader>fq', ':q<CR>')
setmap('n', '<Leader>qa', ':qall<CR>')

setmap('n', '<Leader>bd', ':bd<CR>')

setmap('n', '<Leader>ff', ':FZF<CR>')

setmap('i', '<C-u>', '<esc>viwUea')

setmap('n', '<Leader>bh', ':new<CR>')
setmap('n', '<Leader>bv', ':vnew<CR>')

setmap('n', '<Leader>sh', ':split<CR>')
setmap('n', '<Leader>sv', ':vsplit<CR>')

