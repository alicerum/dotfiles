local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not vim.loop.fs_stat(lazypath) then
  vim.fn.system({
    "git",
    "clone",
    "--filter=blob:none",
    "https://github.com/folke/lazy.nvim.git",
    "--branch=stable", -- latest stable release
    lazypath,
  })
end
vim.opt.rtp:prepend(lazypath)

require("lazy").setup({
  "nvim-treesitter/nvim-treesitter",
  "fatih/vim-go",
  'rust-lang/rust.vim',

  'nvim-tree/nvim-tree.lua',

  'morhetz/gruvbox',

  'nvim-lualine/lualine.nvim',
  'kyazdani42/nvim-web-devicons',
  {
    'TimUntersberger/neogit',
    dependencies = {
      'nvim-lua/plenary.nvim'
    },
  },

  {'nvim-telescope/telescope.nvim', tag = '0.1.1'},
  'nvim-lua/plenary.nvim',
  'BurntSushi/ripgrep',

  'tpope/vim-fugitive',
  'tpope/vim-vinegar',

  'mfussenegger/nvim-dap',

  'neovim/nvim-lspconfig',

  'hrsh7th/cmp-nvim-lsp',
  'hrsh7th/cmp-buffer',
  'hrsh7th/cmp-path',
  'hrsh7th/cmp-cmdline',
  'hrsh7th/nvim-cmp',

  'hrsh7th/cmp-vsnip',
  'hrsh7th/vim-vsnip',

  'nvim-lua/plenary.nvim',
  'lewis6991/gitsigns.nvim',
})
