if(has("termguicolors"))
	set termguicolors
endif

set nocompatible
set encoding=UTF-8
set guifont=Hack\ Nerd\ Font
set tabstop=2
set shiftwidth=2
set expandtab
set ai
set number
set hlsearch
set ruler
set nowrap

highlight Comment ctermfg=green
syntax on

"Automatically download vim-plug if it is not installed in neovim
if empty(glob('~/.local/share/nvim/site/autoload/plug.vim'))
    silent !curl -fLo ~/.local/share/nvim/site/autoload/plug.vim --create-dirs
        \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
    autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
endif

"Run PlugInstall if there are missing plugins
autocmd VimEnter * if len(filter(values(g:plugs), '!isdirectory(v:val.dir)'))
  \| PlugInstall --sync | source $MYVIMRC
\| endif

"Plugins
call plug#begin('~/.vim/plugged')

"Gui
Plug 'mhinz/vim-startify' "Startup ASCII-Art
Plug 'projekt0n/github-nvim-theme' "Color Theme
Plug 'scrooloose/nerdtree' "File tree
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'
Plug 'ryanoasis/vim-devicons' "Icons for files in NERDTree
Plug 'kyazdani42/nvim-web-devicons' "Recommended (for coloured cons)
Plug 'vwxyutarooo/nerdtree-devicons-syntax' "Icon Colors for NERDTree
Plug 'folke/which-key.nvim'

"Languages
Plug 'sheerun/vim-polyglot' "Language pack for syntax and intendation

"Utility
Plug 'camspiers/animate.vim' "Animate windows
Plug 'raimondi/delimitmate' "Auto close brackets etc
Plug 'mattn/emmet-vim' "Emmet support

"Autocompletion
Plug 'neoclide/coc.nvim', {'branch': 'release'}

"Telescope
Plug 'nvim-lua/plenary.nvim'
Plug 'nvim-telescope/telescope.nvim'

"Flotaing Terminal
Plug 'voldikss/vim-floaterm'

"Initialize plugin system
call plug#end()

"Enable custom tabline
let g:airline#extensions#tabline#enabled = 1

"Set the colorscheme
colorscheme github_dark_default

"Set airline theme
let g:airline_theme='transparent'

"Key bindings
nnoremap <SPACE> <Nop>
map <Space> <Leader>

"Animate Window size on direction key press
nnoremap <silent> <Up> :call animate#window_delta_height(10)<CR>
nnoremap <silent> <Down> :call animate#window_delta_height(-10)<CR>
nnoremap <silent> <Right> :call animate#window_delta_width(10)<CR>
nnoremap <silent> <Left> :call animate#window_delta_width(-10)<CR>

:imap jj <Esc>
:tnoremap <Esc> <C-\><C-n>
nnoremap <D-v> "+p

:nnoremap <Leader>w <C-w>

"Tab should expand emment abbreviations
imap <expr> <tab> emmet#expandAbbrIntelligent("\<tab>")

"Telescope binds
nnoremap <leader>ff <cmd>Telescope find_files<cr>
nnoremap <leader>fg <cmd>Telescope live_grep<cr>
nnoremap <leader>fb <cmd>Telescope buffers<cr>

nnoremap <silent> <Leader>t :FloatermToggle<CR>

"Enable which key
lua << EOF
  require("which-key").setup {}
EOF

"Highlight the symbol and its references on cursor hold
autocmd CursorHold * silent call CocActionAsync('highlight')

"Coc extensions
let g:coc_global_extensions = ['coc-json', 'coc-sourcekit', 'coc-git', 'coc-html', 'coc-css', 'coc-tsserver', 'coc-markdownlint', 'coc-psalm', 'coc-sh', 'coc-go', 'coc-lua']

"Max autocomplete window height
set pumheight=20

"Enable NERDTree
let g:webdevicons_enable_nerdtree = 1

call coc#config("suggest.completionItemKindLabels", {
      \   "keyword": "\uf1de",
      \   "variable": "\ue79b",
      \   "value": "\uf89f",
      \   "operator": "\u03a8",
      \   "function": "\u0192",
      \   "reference": "\ufa46",
      \   "constant": "\uf8fe",
      \   "method": "\uf09a",
      \   "struct": "\ufb44",
      \   "class": "\uf0e8",
      \   "interface": "\uf417",
      \   "text": "\ue612",
      \   "enum": "\uf435",
      \   "enumMember": "\uf02b",
      \   "module": "\uf40d",
      \   "color": "\ue22b",
      \   "property": "\ue624",
      \   "field": "\uf9be",
      \   "unit": "\uf475",
      \   "event": "\ufacd",
      \   "file": "\uf723",
      \   "folder": "\uf114",
      \   "snippet": "\ue60b",
      \   "typeParameter": "\uf728",
      \   "default": "\uf29c"
      \ })

" Auto start NERD tree when opening a directory
autocmd VimEnter * if argc() == 1 && isdirectory(argv()[0]) && !exists("s:std_in") | exe 'Telescope find_files' | wincmd p | ene | wincmd p | endif

" Auto start NERD tree if no files are specified
autocmd StdinReadPre * let s:std_in=1
autocmd VimEnter * if argc() == 0 && !exists("s:std_in") | exe 'Telescope find_files' | endif

" Let quit work as expected if after entering :q the only window left open is NERD Tree itself
autocmd bufenter * if (winnr("$") == 1 && exists("b:NERDTree") && b:NERDTree.isTabTree()) | q | endif
