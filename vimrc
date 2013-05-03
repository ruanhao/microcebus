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
set encoding=utf-8
set fileencoding=utf-8
set fileencodings=utf-8,chinese
set nobomb
"Set foldmethod
set foldmethod=marker
"Set backspace to delete
set backspace=indent,eol,start

" Colors
" -----------------------------------------------------------------------------
syntax enable
set background=dark
colorscheme solarized
" Font
set guifont=Consolas:h14:cANSI

" Search
" -----------------------------------------------------------------------------
set incsearch
" Highlight the search terms
set hlsearch
" Ignore case when searching...
set ignorecase
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

" plugin settings
" -----------------------------------------------------------------------------
"Config the NERDTree
nmap <silent> tt :NERDTreeToggle<cr>
"Config the surround
vmap <Leader>s <Plug>Vsurround
vmap <Leader>S <Plug>VSurround
"Config Powerline
let g:Powerline_symbols = 'unicode'
"Config TagList
nmap <silent> tl :TlistToggle<cr>
let Tlist_Show_One_File = 1
"If the taglist window is the last window, then exit vim
let Tlist_Exit_OnlyWindow = 1
let Tlist_Use_Right_Window = 1
