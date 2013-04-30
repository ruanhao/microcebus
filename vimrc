" Pathogen Startup
execute pathogen#infect()


" General Setup
" -----------------------------------------------------------------------------
" Display commands in the bottom right corner as they are typed
set showcmd

"augroupGet out of VI's compatible mode..
set nocompatible

"disable backup
set nobackup
set nowritebackup

"Enable filetype plugin and indent
filetype plugin indent on

"Set to auto read when a file is changed from the outside
set autoread

"Set encoding
"set encoding=chinese
set encoding=utf-8
set fileencoding=utf-8
set fileencodings=utf-8,chinese

set nobomb

"Set foldmethod
set foldmethod=marker

"Set backspace to delete
"set backspace=2
set backspace=indent,eol,start

"Set use mouse
"set ttymouse=xterm
"set mouse=nv


" Colors
" -----------------------------------------------------------------------------
"Enable syntax hl
syntax enable
if has('gui_running')
  set background=light
else
  set background=dark
endif
colorscheme solarized

" Font
"set guifont=Monaco:h14
"set guifont=Courier\ New:h11
set guifont=Consolas:h14:cANSI

" Search
" -----------------------------------------------------------------------------
" I can't live without incremental search
set incsearch

" Highlight the search terms
set hlsearch

" Ignore case when searching...
set ignorecase
" ...but take it into account if there is an uppercase letter in the pattern
set smartcase

" Wrap search when EOF is reached
set wrapscan
" -----------------------------------------------------------------------------

" Indenting and Tabbing
" -----------------------------------------------------------------------------
set autoindent
" Number of spaces used for (auto)indenting
set shiftwidth=4

" Number of spaces to insert for a <tab>
set tabstop=4
set softtabstop=4

" Insert spaces when the <tab> key is pressed
set expandtab

" Enable specific indenting for c-code and others and some nice options for cindenting
set cindent

" Status
" -----------------------------------------------------------------------------
" Always display the status line
set laststatus=2

" format string
set statusline=%1*\File:\ %*%f%1*%5m%*%=\L%-5l\ \C%-4c%5p%%\ [%L\ \lines]

" Show the current editing status
set showmode
" -----------------------------------------------------------------------------



" Key maps
" -----------------------------------------------------------------------------
nmap <C-J> <C-W>j
nmap <C-K> <C-W>k
nmap <C-H> <C-W>h
nmap <C-L> <C-W>l

nmap <silent> <C-C> :tabnew <CR>
nmap <silent> <C-N> :tabnext<CR>
nmap <silent> <C-P> :tabprevious<CR>

"Toggle Menu and Toolbar
set guioptions-=m
set guioptions-=T
nmap <silent> <F2> :if &guioptions =~# 'T' <Bar>
    \set guioptions-=T <Bar>
    \set guioptions-=m <bar>
\else <Bar>
    \set guioptions+=T <Bar>
    \set guioptions+=m <Bar>
\endif<CR>

"gui
"if has('win32')
"  source $VIMRUNTIME/delmenu.vim
"  source $VIMRUNTIME/menu.vim
"endif
" -----------------------------------------------------------------------------

" plugin settings
" -----------------------------------------------------------------------------

"Config the NERDTree
nmap <silent> tt :NERDTreeToggle<cr>

"Config the surround
vmap <Leader>s <Plug>Vsurround
vmap <Leader>S <Plug>VSurround

"Config the Command-T
nmap <silent> <C-F> :CommandT<CR>
let g:CommandTBackspaceMap = '<C-d>'

"Config Powerline
let g:Powerline_symbols = 'unicode'


"Config TagList
nmap <silent> tl :TlistToggle<cr>
let Tlist_Show_One_File = 1

"If the taglist window is the last window, then exit vim
let Tlist_Exit_OnlyWindow = 1   

let Tlist_Use_Right_Window = 1


