filetype plugin on
set encoding=utf-8
set termguicolors
set number
set shell=/bin/bash
set relativenumber
set cursorline
set tabstop=4
set shiftwidth=4
let g:loaded_netrw = 1
let g:loaded_netrwPlugin = 1

"Hello from windows! Dont forget to git sparse-checkout set !/* (with --no-cone) to ignore top level files other than what u add with sparse-checkout add 

highlight! DiagnosticUnderlineError gui=undercurl guisp=Red
highlight! DiagnosticUnderlineWarn gui=undercurl guisp=Yellow
highlight! DiagnosticUnderlineInfo gui=undercurl guisp=Blue
highlight! DiagnosticUnderlineHint gui=undercurl guisp=Cyan

hi Keyword guifg=#fab9d9
hi String guifg=#87faa5
hi Identifier guifg=#ecc2ff
hi Function guifg=#b8b9fc

" Use spaces instead of tabs
set expandtab

highlight Normal guibg=none

" hi Keyword guifg=#
" hi Variable guifg=#
hi CursorLineNr ctermfg=blue
highlight CursorLine guibg=#443454

        
" Plugins will be downloaded under the specified directory.
call plug#begin('~/.config/nvim/plugged')

"LSP BS
Plug 'neovim/nvim-lspconfig'
Plug 'nvim-lua/plenary.nvim'
Plug 'hrsh7th/cmp-nvim-lsp'
Plug 'hrsh7th/cmp-buffer'
Plug 'hrsh7th/cmp-path'
Plug 'hrsh7th/cmp-cmdline'
Plug 'hrsh7th/nvim-cmp'
Plug 'hrsh7th/cmp-vsnip'
Plug 'hrsh7th/vim-vsnip'

Plug 'norcalli/nvim-colorizer.lua' 
Plug 'purescript-contrib/purescript-vim'
Plug 'enomsg/vim-haskellConcealPlus'
"Plug 'dense-analysis/ale'
Plug 'neovimhaskell/haskell-vim'
Plug 'pbrisbin/vim-syntax-shakespeare'
Plug 'mattn/emmet-vim'
Plug 'pmizio/typescript-tools.nvim'
Plug 'othree/yajs.vim'
Plug 'MaxMEllon/vim-jsx-pretty'
Plug 'pbrisbin/vim-syntax-shakespeare'

"Plug 'preservim/nerdtree'
Plug 'nvim-tree/nvim-tree.lua'
"Plug 'nvim-treesitter/nvim-treesitter', {'do': ':TSUpdate'}
"Plug 'windwp/nvim-ts-autotag', { 'do': ':TSUpdate' }
Plug 'tpope/vim-surround'
Plug 'nvim-lualine/lualine.nvim'
Plug 'nvim-tree/nvim-web-devicons' "lualine
Plug 'onsails/lspkind-nvim'

Plug 'windwp/nvim-autopairs'
Plug 'alvan/vim-closetag'

"Plug 'HiPhish/rainbow-delimiters.nvim' "activate this when u can change the ugly fucking default colors lol
Plug 'akinsho/bufferline.nvim', { 'tag': '*' }
Plug 'williamboman/mason.nvim'
Plug 'williamboman/mason-lspconfig.nvim'
Plug 'terrortylor/nvim-comment'

"installed this for vue remove if it causes shit
"Plug 'neoclide/coc.nvim', {'branch': 'release'}
"Plug 'yaegassy/coc-volar', { 'do': 'yarn install --frozen-lockfile' }
"Plug 'yaegassy/coc-volar-tools', { 'do': 'yarn install --frozen-lockfile' }

call plug#end()

" To make hls work, install hls using ghcup (set ghc 9.0.2  as default if necessary).#
" Only use ghcup to manage haskell tooling, so only (ONLY) install ghcup with ur distro's pm, do NOT install cabal/stack/hls with it, if u did, uninstall them and reinstall with ghcup
" Do not install haskell-language-server-wrapper with AUR or Pacman (the hls site says to do this shit ignore it it will NOT work), if you have it, GHC or Cabal installed with Pacman, uninstall them and reinstall with ghcup.
" Binaries will be installed in ~/.ghcup/bin/ so make sure to add this directory to your path to be able to use Cabal and GHC.
" Double check that Neovim inherits it correctly or else haskell-language-server-wrapper won't spawn
" Install any missing packages with 'cabal install --lib' (verify theyre installed with ghci's ':show packages command', IGNORE 'cabal list --installed' only use ghci)
" Make sure to populate the yaml thing by running the 'haskell-language-server-wrapper' command on bash in the same directory as ur .hs files else it wont work
" ideally u shouldnt install stack, even with ghcup it sucks ass and causes problems by installing its own shit and not giving a damn about ur configs
" Hls should be working correctly.
"
" If you need to use Stack to work with Yesod, the best course of action is to
" obviously use ghci, but make sure ~/.stack/config.yaml has 'system-ghc:
" true:' and stack.yaml (~/.stack/global-project/stack.yaml) has 'system-ghc:
" true' 'resolver: lts-21.24' (or something like this) and if were using
" ghc-9.0.2 we also want 'compiler: ghc-9.0.2', this is the so-called global
" configuration to prevent stack from wanting to do its own shit and wreak
" havoc in our ghci install
"
" Now for the config thats specific to the Yesod project, since .../stack.yaml
" has precendence over the global ~/.cabal/stack.yaml etc, we also need to
" specify these things here, otherwise stack will want to do its own garbage
" system-ghc: true
" resolver: lts-21.24
" compiler: ghc-9.0.2
" allow-newer: true (this one isnt strictly necessary however when trying to build it
" will prevent from erroring out the shit since the modules will probably nto
" be the exact fucking version that the configs demand so it will make ur life
" easier)
" After all this circus follow the yesod quickstart steps and wait for it to
" compile, stack exec -- yesod devel should work without stack fucking up
" anything

let $PATH = "/home/serna/.ghcup/bin:" . $PATH


" Enable autocompletion
set omnifunc=syntaxcomplete#Complete

" Use GHC installed with ghcup
let g:haskell_compiler_type = 'ghcup'

" Specify the path to the Haskell Language Server executable
let g:haskell_language_server = 'haskell-language-server-wrapper'

" Enable Haskell syntax highlighting and conceal
autocmd FileType haskell setlocal conceallevel=2 concealcursor=niv

" Enable ALE for linting (optional but recommended)
let g:ale_linters = {
      \ 'haskell': ['ghc'],
      \ }
let g:ale_fixers = {
      \ 'haskell': ['brittany', 'ormolu'],
      \ }
autocmd BufRead,BufNewFile *.prolog set filetype=prolog

"HTML AUTOCOMPLETE BS
" filenames like *.xml, *.html, *.xhtml, ...
" These are the file extensions where this plugin is enabled.
"
let g:closetag_filenames = '*.html,*.xhtml,*.phtml'

" filenames like *.xml, *.xhtml, ...
" This will make the list of non-closing tags self-closing in the specified files.
"
let g:closetag_xhtml_filenames = '*.xhtml,*.jsx'

" filetypes like xml, html, xhtml, ...
" These are the file types where this plugin is enabled.
"
let g:closetag_filetypes = 'html,xhtml,phtml'

" filetypes like xml, xhtml, ...
" This will make the list of non-closing tags self-closing in the specified files.
"
let g:closetag_xhtml_filetypes = 'xhtml,jsx'

" integer value [0|1]
" This will make the list of non-closing tags case-sensitive (e.g. `<Link>` will be closed while `<link>` won't.)
"
let g:closetag_emptyTags_caseSensitive = 1

" dict
" Disables auto-close if not in a "valid" region (based on filetype)
"
let g:closetag_regions = {
    \ 'typescript.tsx': 'jsxRegion,tsxRegion',
    \ 'javascript.jsx': 'jsxRegion',
    \ 'typescriptreact': 'jsxRegion,tsxRegion',
    \ 'javascriptreact': 'jsxRegion',
    \ }

" Shortcut for closing tags, default is '>'
"
let g:closetag_shortcut = '>'

" Add > at current position without closing the current tag, default is ''
"
let g:closetag_close_shortcut = '<leader>>'

" Enables closing tags for React fragments -> <></> for all supported file types
"
let g:closetag_enable_react_fragment = 1

"HTML AUTOCOMPLETE BS


lua <<EOF 

require("nvim-autopairs").setup {}
require("bufferline").setup{}
require('mason').setup()
require("nvim-tree").setup()

require'nvim-web-devicons'.setup {
    default = true; -- Enable default icons
}
require("nvim-web-devicons").refresh()
require("nvim-web-devicons").get_icons()

--[[
require'nvim-treesitter.configs'.setup {
    ensure_installed = "all", -- or specify languages as a list
    highlight = {
        enable = true, -- Enable Tree-sitter highlighting
        additional_vim_regex_highlighting = false,
    },
}


require('nvim-ts-autotag').setup({
  opts = {
    -- Defaults
    enable_close = true, -- Auto close tags
    enable_rename = true, -- Auto rename pairs of tags
    enable_close_on_slash = false -- Auto close on trailing </
  },
  -- Also override individual filetype configs, these take priority.
  -- Empty by default, useful if one of the "opts" global settings
  -- doesn't work well in a specific filetype
})
]]
--Lualine bs 
require('lualine').setup {
  options = {
    icons_enabled = true,
    theme = 'dracula',
    component_separators = { left = '', right = ''},
    section_separators = { left = '', right = ''},
    disabled_filetypes = {
      statusline = {},
      winbar = {},
    },
    ignore_focus = {},
    always_divide_middle = true,
    globalstatus = false,
    refresh = {
      statusline = 1000,
      tabline = 1000,
      winbar = 1000,
    }
  },
  sections = {
    lualine_a = {'mode'},
    lualine_b = {'branch', 'diff', 'diagnostics'},
    lualine_c = {'filename'},
    lualine_x = {'encoding', 'fileformat', 'filetype'},
    lualine_y = {'progress'},
    lualine_z = {'location'}
  },
  inactive_sections = {
    lualine_a = {},
    lualine_b = {},
    lualine_c = {'filename'},
    lualine_x = {'location'},
    lualine_y = {},
    lualine_z = {}
  },
  tabline = {},
  winbar = {},
  inactive_winbar = {},
  extensions = {}
}


  require'colorizer'.setup()

  -- Set up nvim-cmp.
  local cmp = require'cmp'

  cmp.setup({
    snippet = {
      -- REQUIRED - you must specify a snippet engine
      expand = function(args)
        vim.fn["vsnip#anonymous"](args.body) -- For `vsnip` users.
        -- require('luasnip').lsp_expand(args.body) -- For `luasnip` users.
        -- require('snippy').expand_snippet(args.body) -- For `snippy` users.
        -- vim.fn["UltiSnips#Anon"](args.body) -- For `ultisnips` users.
      end,
    },
    window = {
      -- completion = cmp.config.window.bordered(),
      -- documentation = cmp.config.window.bordered(),
    },
    mapping = cmp.mapping.preset.insert({
      ['<C-b>'] = cmp.mapping.scroll_docs(-4),
      ['<C-f>'] = cmp.mapping.scroll_docs(4),
      ['<C-Space>'] = cmp.mapping.complete(),
      ['<C-e>'] = cmp.mapping.abort(),
      ['<CR>'] = cmp.mapping.confirm({ select = true }), -- Accept currently selected item. Set `select` to `false` to only confirm explicitly selected items.
    }),
    sources = cmp.config.sources({
      { name = 'nvim_lsp' },
      { name = 'vsnip' }, -- For vsnip users.
      -- { name = 'luasnip' }, -- For luasnip users.
      -- { name = 'ultisnips' }, -- For ultisnips users.
      -- { name = 'snippy' }, -- For snippy users.
    }, {
      { name = 'buffer' },
    })
  })

  -- Set configuration for specific filetype.
  cmp.setup.filetype('gitcommit', {
    sources = cmp.config.sources({
      { name = 'git' }, -- You can specify the `git` source if [you were installed it](https://github.com/petertriho/cmp-git).
    }, {
      { name = 'buffer' },
    })
  })

  -- Use buffer source for `/` and `?` (if you enabled `native_menu`, this won't work anymore).
  cmp.setup.cmdline({ '/', '?' }, {
    mapping = cmp.mapping.preset.cmdline(),
    sources = {
      { name = 'buffer' }
    }
  })

  -- Use cmdline & path source for ':' (if you enabled `native_menu`, this won't work anymore).
  cmp.setup.cmdline(':', {
    mapping = cmp.mapping.preset.cmdline(),
    sources = cmp.config.sources({
      { name = 'path' }
    }, {
      { name = 'cmdline' }
    })
  })

local lspconfig = require('lspconfig')

lspconfig.purescriptls.setup {
  on_attach = on_attach,
  settings = {
    purescript = {
      addSpagoSources = true -- e.g., any purescript language-server config here
    }
  },
  flags = {
    debounce_text_changes = 150,
  }
}
  -- set up lspkind 
  require('lspkind').init({
    -- DEPRECATED (use mode instead): enables text annotations
    --
    -- default: true
    -- with_text = true,

    -- defines how annotations are shown
    -- default: symbol
    -- options: 'text', 'text_symbol', 'symbol_text', 'symbol'
    mode = 'symbol_text',

    -- default symbol map
    -- can be either 'default' (requires nerd-fonts font) or
    -- 'codicons' for codicon preset (requires vscode-codicons font)
    --
    -- default: 'default'
    preset = 'codicons',

    -- override preset symbols
    --
    -- default: {}
    symbol_map = {
      Text = "󰉿",
      Method = "󰆧",
      Function = "󰊕",
      Constructor = "",
      Field = "󰜢",
      Variable = "󰀫",
      Class = "󰠱",
      Interface = "",
      Module = "",
      Property = "󰜢",
      Unit = "󰑭",
      Value = "󰎠",
      Enum = "",
      Keyword = "󰌋",
      Snippet = "",
      Color = "󰏘",
      File = "󰈙",
      Reference = "󰈇",
      Folder = "󰉋",
      EnumMember = "",
      Constant = "󰏿",
      Struct = "󰙅",
      Event = "",
      Operator = "󰆕",
      TypeParameter = "",
    },
})

  -- Set up lspconfig.
  local capabilities = require('cmp_nvim_lsp').default_capabilities()
  -- Replace <YOUR_LSP_SERVER> with each lsp server you've enabled.
  require('lspconfig')['clangd'].setup {
    capabilities = capabilities
  }

  require('lspconfig')['denols'].setup {
  on_attach = on_attach,
  root_dir = require'lspconfig'.util.root_pattern("deno.json", "deno.jsonc"),
  filetypes = {
    'javascript',
    'javascriptreact',
    'javascript.jsx',
    'typescript',
    'typescriptreact',
    'typescript.tsx',
    'markdown',
  },
  init_options = {
    config = './deno.jsonc',
    lint = true,
  },
}

  require('lspconfig')['ts_ls'].setup { 
  cmd = { "C:\\Users\\Giga\\AppData\\Roaming\\npm\\typescript-language-server.cmd", "--stdio" },
  on_attach = on_attach,
  filetypes = { "typescript", "typescriptreact", "javascript", "javascriptreact" },
  capabilities = capabilities,
  root_dir = require'lspconfig'.util.root_pattern("package.json"),
  single_file_support = false,  -- This disables single file support for the TypeScript language server
}

   require('lspconfig')['html'].setup {
    capabilities = capabilities
  } 
   
   require('lspconfig')['cssls'].setup {
    capabilities = capabilities
  } 

   require('lspconfig')['jdtls'].setup {
    capabilities = capabilities
  } 

   require('lspconfig')['purescriptls'].setup{
    capabilities = capabilities
  }

  require('lspconfig')['hls'].setup{
    filetypes = { 'haskell', 'hs' , 'lhaskell', 'cabal' },
    capabilities = capabilities

  }
  

require('lspconfig')['sqlls'].setup{
  capabilities = capabilities,
  root_dir = function() return vim.loop.cwd() end,
}


  require('lspconfig')['prolog_ls'].setup{
    capabilities = capabilities
  }

   require('lspconfig')['scheme_langserver'].setup{
    capabilities = capabilities
  } 

  require'lspconfig'.asm_lsp.setup{
   cmd = {"/home/serna/.cargo/bin/asm-lsp"},
    filetypes = {"asm", "vmasm"},
    root_dir = function() return vim.loop.cwd() end,
    capabilities = capabilities,
    on_init = function(client)
        -- Disable diagnostics (errors and warnings)
        client.handlers["textDocument/publishDiagnostics"] = vim.lsp.with(
            vim.lsp.diagnostic.on_publish_diagnostics, {
                severity_sort = true,
                underline = true,
                signs = true,
            }
        )
    end,
  }

   require 'lspconfig'.volar.setup { -- btw current volar is unusable for unknown reasons use this version instead: MasonInstall vue-language-server@1.8.27  
    -- install with npm @vue/typescript-plugin and @vue/language-server globally (and whatever bs the warnings tell u to), also make sure to use tsserver if ur using 1.8.27 because thats what that version integrates to
    --u might also (???) install typescript-language-server (from npm -g) and vue-language-server which are different from the previously mentioned ones because apparently the vue ecosystem is a clusterfuck, good luck
       filetypes = { 'typescript', 'javascript', 'javascriptreact', 'typescriptreact', 'vue', 'json' },
       init_options = {
      typescript = {
      tsdk = "C:\\Users\\Giga\\AppData\\Roaming\\npm\\node_modules\\typescript\\lib"
      -- Alternative location if installed as root:
      -- tsdk = '/usr/local/lib/node_modules/typescript/lib'
    }
  }
}

EOF
