if(has("termguicolors"))
	set termguicolors
endif

set nocompatible
set encoding=UTF-8
set guifont=Hack\ Nerd\ Font

"Plugins
call plug#begin('~/.vim/plugged')

Plug 'scrooloose/nerdtree' "file tree
Plug 'camspiers/animate.vim' "animate windows
Plug 'HenryNewcomer/vim-theme-papaya' "theme
Plug 'sheerun/vim-polyglot' "Language Pack
Plug 'raimondi/delimitmate' "Auto Close Brackets etc
Plug 'mattn/emmet-vim' "emmet
Plug 'ryanoasis/vim-devicons' "Icons for files in NERDTree
Plug 'vwxyutarooo/nerdtree-devicons-syntax' "Icon Colors for NERDTree
Plug 'pangloss/vim-javascript'
Plug 'leafgarland/typescript-vim'
Plug 'maxmellon/vim-jsx-pretty'
Plug 'prettier/vim-prettier', { 'do': 'yarn install' }

" Initialize plugin system
call plug#end()

"set the colorscheme
colorscheme papaya

"key bindings
nnoremap <silent> <Up>    :call animate#window_delta_height(10)<CR>
nnoremap <silent> <Down>  :call animate#window_delta_height(-10)<CR>
nnoremap <silent> <Right>  :call animate#window_delta_width(10)<CR>
nnoremap <silent> <Left> :call animate#window_delta_width(-10)<CR>
:imap jj <Esc>
:tnoremap <Esc> <C-\><C-n>
nnoremap <D-v> "+p
imap <expr> <tab> emmet#expandAbbrIntelligent("\<tab>")

"Geohotz config
syntax on
set tabstop=2
set shiftwidth=2
set expandtab
set ai
set number
set hlsearch
set ruler
highlight Comment ctermfg=green

"Auto Format
let g:prettier#autoformat = 0
autocmd BufWritePre *.js,*.jsx,*.mjs,*.ts,*.tsx,*.css,*.less,*.scss,*.json,*.graphql,*.md,*.vue,*.yaml,*.html PrettierAsync

"Enable NERDTree
let g:webdevicons_enable_nerdtree = 1

"Open NERDTree on startup and switch panes
autocmd VimEnter * NERDTree
autocmd VimEnter * wincmd p
