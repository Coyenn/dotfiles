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
Plug 'kyazdani42/nvim-web-devicons' " Recommended (for coloured icons)
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'
Plug 'mhinz/vim-startify'

" Initialize plugin system
call plug#end()

"Enable custom tabline
let g:airline#extensions#tabline#enabled = 1
let g:airline#extensions#tabline#left_sep = ' '
let g:airline#extensions#tabline#left_alt_sep = '|'
let g:airline#extensions#tabline#formatter = 'unique_tail'

"set the colorscheme
colorscheme papaya

"set airline theme
let g:airline_theme='transparent'

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

"Custom Header on Startup page
let s:header = [
      \ '',
\ '      ____________',
\ '     /\  ________ \',
\ '    /  \ \______/\ \',
\ '   / /\ \ \  / /\ \ \',
\ '  / / /\ \ \/ / /\ \ \',
\ ' / / /__\_\/ / /__\_\ \',
\ '/ /_/_______/ /________\',
\ '\ \ \______ \ \______  /',
\ ' \ \ \  / /\ \ \  / / /',
\ '  \ \ \/ / /\ \ \/ / /',
\ '   \ \/ / /__\_\/ / /',
\ '    \  / /______\/ /',
\ '     \/___________/',
      \ '',
      \ ]

let s:footer = []

function! s:center(lines) abort
  let longest_line   = max(map(copy(a:lines), 'strwidth(v:val)'))
  let centered_lines = map(copy(a:lines),
        \ 'repeat(" ", (&columns / 2) - (longest_line / 2)) . v:val')
  return centered_lines
endfunction

let g:startify_custom_header = s:center(s:header)
let g:startify_custom_footer = s:center(s:footer)
