if(has("termguicolors"))
	set termguicolors
endif

set nocompatible
set encoding=UTF-8
set guifont=Hack\ Nerd\ Font

"Automatically download vim-plug if it is not installed in neovim
if empty(glob('~/.local/share/nvim/site/autoload/plug.vim'))
    silent !curl -fLo ~/.local/share/nvim/site/autoload/plug.vim --create-dirs
        \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
    autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
endif

" Run PlugInstall if there are missing plugins
autocmd VimEnter * if len(filter(values(g:plugs), '!isdirectory(v:val.dir)'))
  \| PlugInstall --sync | source $MYVIMRC
\| endif

"Plugins
call plug#begin('~/.vim/plugged')

"Gui
Plug 'mhinz/vim-startify' "Startup ASCII-Art
Plug 'sainnhe/gruvbox-material' "Color scheme
Plug 'scrooloose/nerdtree' "File tree
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'
Plug 'ryanoasis/vim-devicons' "Icons for files in NERDTree
Plug 'kyazdani42/nvim-web-devicons' "Recommended (for coloured cons)
Plug 'vwxyutarooo/nerdtree-devicons-syntax' "Icon Colors for NERDTree

"Languages
Plug 'sheerun/vim-polyglot' "Language pack for syntax and intendation
Plug 'pangloss/vim-javascript'
Plug 'leafgarland/typescript-vim'

"Utility
Plug 'camspiers/animate.vim' "Animate windows
Plug 'raimondi/delimitmate' "Auto close brackets etc
Plug 'mattn/emmet-vim' "Emmet support

"Autocompletion
Plug 'neoclide/coc.nvim', {'branch': 'release'}

"Telescope
Plug 'nvim-lua/plenary.nvim'
Plug 'nvim-telescope/telescope.nvim'

"Initialize plugin system
call plug#end()

"Enable custom tabline
let g:airline#extensions#tabline#enabled = 1

"Set the colorscheme
colorscheme gruvbox-material

"Set airline theme
let g:airline_theme='transparent'

"Key bindings
let mapleader = "\<Space>" "Set leader to space

"Animate Window size on direction key press
nnoremap <silent> <Up> :call animate#window_delta_height(10)<CR>
nnoremap <silent> <Down> :call animate#window_delta_height(-10)<CR>
nnoremap <silent> <Right> :call animate#window_delta_width(10)<CR>
nnoremap <silent> <Left> :call animate#window_delta_width(-10)<CR>

:imap jj <Esc>
:tnoremap <Esc> <C-\><C-n>
nnoremap <D-v> "+p

"Tab should expand emment abbreviations
imap <expr> <tab> emmet#expandAbbrIntelligent("\<tab>")

"Telescope binds
nnoremap <leader>ff <cmd>Telescope find_files<cr>
nnoremap <leader>fg <cmd>Telescope live_grep<cr>
nnoremap <leader>fb <cmd>Telescope buffers<cr>

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

"Highlight the symbol and its references on cursor hold
autocmd CursorHold * silent call CocActionAsync('highlight')

"Coc extensions
let g:coc_global_extensions = ['coc-json', 'coc-git', 'coc-html', 'coc-css', 'coc-tsserver', 'coc-markdownlint', 'coc-psalm', 'coc-sh']

"Enable NERDTree
let g:webdevicons_enable_nerdtree = 1

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

"Function for centering the ASCII Art
function! s:center(lines) abort
  let longest_line   = max(map(copy(a:lines), 'strwidth(v:val)'))
  let centered_lines = map(copy(a:lines),
        \ 'repeat(" ", (&columns / 2) - (longest_line / 2)) . v:val')
  return centered_lines
endfunction

let g:startify_custom_header = s:center(s:header)
let g:startify_custom_footer = s:center(s:footer)
