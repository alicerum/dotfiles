vim.g.mapleader=","
vim.g.maplocalleader="."

require('plugins')
require('treesitter')
require('visuals')
require('haskell')
require('golang')
require('lsp')
require('luacmp')
require('mappings')
require('bottomline')
require('barbar')
require('signs')

local o = vim.o
local wo = vim.wo
local bo = vim.bo

-- o.showtabline = 2
o.backup = false
o.writebackup = false
o.splitbelow = true
o.clipboard = 'unnamedplus'

wo.number = true
wo.signcolumn = 'yes:1'

vim.g.netrw_fastbrowse = 0
vim.g.netrw_liststyle = 3

