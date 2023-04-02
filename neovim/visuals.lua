vim.o.background = 'dark'

vim.g.gruvbox_invert_selection=0
vim.g.gruvbox_contrast_dark='medium'
vim.g.gruvbox_italic=1

vim.opt.termguicolors = true
vim.cmd('colorscheme gruvbox')
vim.cmd('highlight clear SignColumn')

-- empty setup using defaults
require("nvim-tree").setup()

-- OR setup with some options
require("nvim-tree").setup({
  sort_by = "case_sensitive",
  renderer = {
    group_empty = true,
  },
  filters = {
    dotfiles = true,
  },
})

