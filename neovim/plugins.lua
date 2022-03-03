local Plug = vim.fn['plug#']
vim.call('plug#begin', '~/.config/nvim/plugged')

  Plug('nvim-treesitter/nvim-treesitter', {['do'] = ':TSUpdate'})
  Plug('junegunn/fzf', {['do'] = ':call fzf#install()' })

  Plug('fatih/vim-go', {['do'] = ':GoUpdateBinaries' })

  Plug('morhetz/gruvbox')

  Plug('nvim-lualine/lualine.nvim')
  Plug('romgrk/barbar.nvim')
  Plug('kyazdani42/nvim-web-devicons')

  Plug('tpope/vim-fugitive')
  Plug('tpope/vim-vinegar')

  Plug('neovim/nvim-lspconfig')

  Plug('neovim/nvim-lspconfig')
  Plug('hrsh7th/cmp-nvim-lsp')
  Plug('hrsh7th/cmp-buffer')
  Plug('hrsh7th/cmp-path')
  Plug('hrsh7th/cmp-cmdline')
  Plug('hrsh7th/nvim-cmp')

  Plug('hrsh7th/cmp-vsnip')
  Plug('hrsh7th/vim-vsnip')

  Plug('nvim-lua/plenary.nvim')
  Plug('lewis6991/gitsigns.nvim')

vim.call('plug#end')

