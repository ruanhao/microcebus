" File    : vimrc
" Modifier: Hao
" Purpose : VIM configuration file
" Modified: 2013.6.3
" Ref     : http://edyfox.codecarver.org/html/_vimrc_for_beginners.html

" Pathogen Startup
execute pathogen#infect()

" General Setup
" -----------------------------------------------------------------------------

" Set tab completion mode
set wildmenu
set wildmode=full

" Set history
set history=200

" Set number
set nu

" Set mouse mode
set mouse=a

" Display commands in the bottom right corner as they are typed
set showcmd

" Do not use VI mode
set nocompatible

" Disable backup file
set nobackup

" I want to use swap file 
" Ref: http://vim.wikia.com/wiki/Remove_swap_and_backup_files_from_your_working_directory
set writebackup
set directory=/tmp,./.swaps,.

" This equals to:
" filetype on
" filetype plugin on
" filetype indent on
filetype plugin indent on

" Update immediately if the file is changed from the outside
set autoread

" Set encoding
set encoding=utf-8
set fileencodings=ucs-bom,utf-8,cp936,gb18030,big5,euc-jp,euc-kr,latin1

" Remove BOM header
set nobomb

" Set foldmethod
set foldmethod=syntax
" Set no folding initially, but can be activated when hit 'zm' or something
set nofoldenable

" Set backspace to delete
set backspace=indent,eol,start

" Set paste toggle
set pastetoggle=<f11>

" Colors
" -----------------------------------------------------------------------------

" Enable syntax
syntax enable

" I like dark background
set background=dark

" Molokai is good
colorscheme molokai

" Font
"set guifont=Consolas:h14:cANSI

" Search
" -----------------------------------------------------------------------------

" Incremental search
set incsearch

" Highlight the search terms
set hlsearch

" Ignore case when searching
set ignorecase

" Set smartcase, e.g:
" /the can go to 'The' while /The can not go to 'the'
set smartcase

" Wrap search when EOF is reached
set wrapscan

" Set max text width when wrap
set wrap
set textwidth=80

" Indenting and Tabbing
" -----------------------------------------------------------------------------

set autoindent

" Number of spaces used for (auto)indenting
set shiftwidth=4

" Number of spaces to insert for a <tab>
set tabstop=4

" Insert spaces when the <tab> key is pressed
set expandtab

" Enable specific indenting for c-code 
" set cindent

" Status
" -----------------------------------------------------------------------------

" Always display the status line
set laststatus=2

" format string
set statusline=%1*\File:\ %*%f%1*%5m%*%=\L%-5l\ \C%-4c%5p%%\ [%L\ \lines]

" Show the current editing status
set showmode

" Key maps
" -----------------------------------------------------------------------------

" Window switch
nmap <C-J> <C-W>j
nmap <C-K> <C-W>k
nmap <C-H> <C-W>h
nmap <C-L> <C-W>l

" Table utilities
"nmap <silent> <C-C> :tabnew <CR>
nmap <silent> <C-N> :tabnext<CR>
nmap <silent> <C-P> :tabprevious<CR>

" Plugin settings
" -----------------------------------------------------------------------------

" Config the NERDTree
nmap <silent> tt :NERDTreeToggle<cr>

" Config TagList ( I don't like it actually )
"nmap <silent> tl :TlistToggle<cr>
" If the Taglist window is the last window, then exit VIM
"let Tlist_Exit_OnlyWindow = 1
"let Tlist_Use_Right_Window = 1
"let Tlist_Show_One_File = 1

" Config vimerl
let g:erlangHighlightBif=1
let g:erlangCheckFile="~/.vim/bundle/vimerl/compiler/erlang_check_file.erl"
"let g:erlangFoldSplitFunction=1
" Maybe ManPath should be changed accordingly
let g:erlangManPath="/usr/lib/erlang/man"

" Create mappings to quickly traverse Vim's lists
nnoremap <silent> [b :bprevious<CR>
nnoremap <silent> ]b :bnext<CR>
nnoremap <silent> [B :bfirst<CR>
nnoremap <silent> ]B :blast<CR>

" Avoid the cursor keys
cnoremap <C-p> <Up>
cnoremap <C-n> <Down>

"noremap <Leader>n nzz
"noremap <Leader>N Nzz

" Config Ctag
"nnoremap <f5> :!ctags -R<CR>
"autocmd BufWritePost * call system("ctags -R")
