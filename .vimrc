" Best Goddamn vimrc in the whole world.
" Author: Seth House <seth@eseth.com>
" Version: 1.0.5
" Modified: 2006-12-17
" Revamped for Vim 7 (tabs!) - will output a few non-critical errors for old versions.
" For more information type :help followed by the command.

set nocompatible                "cp:    turns off strct vi compatibility

" {{{ Search

set incsearch                   "is:    automatically begins searching as you type
set ignorecase                  "ic:    ignores case when pattern matching
set smartcase                   "scs:   ignores ignorecase when pattern contains uppercase characters
set hlsearch                    "hls:   highlights search results
" Use ctrl-n to unhighlight search results in normal mode:
nmap <silent> <C-N> :silent noh<CR>

" }}}
" {{{ Line Wrap

set backspace=indent,eol,start  "bs:    allows you to backspace over the listed character types
set linebreak                   "lbr:   causes vim to not wrap text in the middle of a word
set wrap                        "wrap:  wraps lines by default
" Toggle line wrapping in normal mode:
nmap <silent> <C-P> :set nowrap!<CR>:set nowrap?<CR>

" }}}
" {{{ Programming

syntax on                       "syn:   syntax highlighting
set showmatch                   "sm:    flashes matching brackets or parentheses

set cindent                     "cin:   enables the second-most configurable indentation (see :help C-indenting).
set cinoptions=l1,c4,(s,U1,w1,m1,j1

set expandtab                   "et:    uses spaces instead of tab characters
set smarttab                    "sta:   helps with backspacing because of expandtab
set tabstop=3                   "ts:    number of spaces that a tab counts for
set shiftwidth=3                "sw:    number of spaces to use for autoindent

set listchars=tab:>-,eol:$      "lcs:   makes finding tabs easier during :set list
set lazyredraw                  "lz:    will not redraw the screen while running macros (goes faster)

" }}}
" {{{ Folding (spacebar toggles a fold, zi toggles all folding)

set foldmethod=marker           "fdm:   looks for patterns of triple-braces in a file
noremap  <silent>  <space> :exe 'silent! normal! za'.(foldlevel('.')?'':'l')<cr>

" }}}
" {{{ Backups


"set backup                      "bk:    makes a backup copy of every file you write to
"set backupdir=~/.vim/tmp        "bdir:  this is not a secure place to store a copy of every file you edit!
"au FileType crontab set nobackup " crontab won't save otherwise due to the above options

" }}}
" {{{ Menu completion

set wildmenu                    "wmnu:  enhanced ex command completion
set wildmode=longest:full,list:full  "wim:   helps wildmenu auto-completion
set wildignore+=lost+found/**,*.o,*.dbo,*.out,_*

" }}}
" {{{ Multi-buffer editing

set switchbuf=usetab            "swb:   Jumps to first window or tab that contains specified buffer instead of duplicating an open window
set showtabline=2               "stal:  always display the tab-bar at the top. Use :tab ball or invoke Vim with -p
set hidden                      "hid:   allows opening a new buffer in place of an existing one without first saving the existing one

" Allows ctrl-h,l to move left and right between tabs
nmap <C-H> gT
nmap <C-L> gt

" ctrl-j,k will move up or down between split buffers. use ctrl-w, ctrl-_ to maximize, or ctrl-o to show only the current buffer
nmap <C-J> <C-W>j
nmap <C-K> <C-W>k

" I'm not sure why Vim displays one line by default when 'maximizing' a split window with ctrl-_
set winminheight=0              "wmh:   the minimal height of any non-current window
set winminwidth=0               "wmw:   the minimal width of any non-current window

" Earlier Vims did not support tabs. Below is a vertical-tab-like cludge. Use :ball or invoke Vim with -o
if version < 700

    " ctrl-j,k will move up or down between split buffers and maximize the current buffer
    nmap <C-J> <C-W>j<C-W>_
    nmap <C-K> <C-W>k<C-W>_

endif

" Map Ctrl-\ to cycle to the next buffer
nmap <C-\> <C-W><C-W>
" }}}
" {{{ HUD and Status Info

set number                      "nu:    numbers lines
set numberwidth=4               "nuw:   the default is 4, however GNU screen uses 8 like old Vi and there's no way to change it in screen :-(
set showmode                    "smd:   shows current vi mode in lower left
set cursorline                  "cul:   highlights the current line
set showcmd                     "sc:    shows typed commands
set cmdheight=2                 "ch:    make a little more room for error messages
set scrolloff=2                 "so:    places a couple lines between the current line and the screen edge
set sidescrolloff=2             "siso:  places a couple lines between the current column and the screen edge
set laststatus=2                "ls:    makes the status bar always visible
set ttyfast                     "tf:    improves redrawing for newer computers
set viminfo='500,f1,:100,/100   "vi:    For a nice, huuuuuge viminfo file.

" FIXME: Not sure why fileencoding below doesn't always display...
set statusline=#%n\ %<%F%m%r\ %w\ %y\ \ <%{&fileencoding},%{&fileformat}>%=%l,%c%V\ of\ %L\ \ \ \ \ \ \ %P

" }}}
" {{{ Color
"     All coloring options are for the non-GUI Vim (see :help cterm-colors).
"
"     Delving into the labrynth that is terminal codes (terminfo and termcap)
"     is not for the feint of heart.
"     Vim can really benefit from 16-color terminals, but it seems that most
"     modern terminals default to 8 colors (compatibility's sake?).
"     You can probably export your term as xterm-16color instead of the
"     oft-used xterm-color, or you can tell Vim how many colors are available
"     with the following line.
set t_Co=16
"hi Constant	ctermfg=white
hi Identifier	ctermfg=white 
hi Function ctermfg=white 

" The default fold color is too bright and looks too much like the statusline
hi Folded cterm=bold ctermfg=8 ctermbg=0

" I love the new CursorLine, but terminal underlining kicks legibility in the nuts.
" Unfortunately, bolding barely makes any difference at all... :-(
hi CursorLine cterm=bold

" 'Greys-out' the non-current StatusLine in a split
hi StatusLineNC cterm=bold ctermbg=8 ctermfg=0

" Helps set apart the TabLine from the StatusLine in split (and maximized) windows
hi TabLineFill cterm=none ctermbg=12

" Show syntax highlighting groups for word under cursor
nmap <C-S-P> :call <SID>SynStack()<CR>
function! <SID>SynStack()
  if !exists("*synstack")
    return
  endif
  echo map(synstack(line('.'), col('.')), 'synIDattr(v:val, "name")')
endfunc

" }}}
" {{{ :Explore mode

let g:netrw_hide=1          " Use the hiding list
" Hide the following file patterns (change to suit your needs):
" (I don't know what the fuck \~$ is, but file hiding seems to break without it
" appearing first in the list...)
let g:netrw_list_hide='\~$,\.pyc$,__init__\.py$'

let g:netrw_winsize=30
"nmap <silent> <C-T> :Vexplore!<CR>
nmap <silent> <C-T> :tabnew<CR>:Explore<CR>

" }}}
" {{{ Autocommands, and file type specific settings

" Remember last position in file
au BufReadPost * if line("'\"") > 0 && line("'\"") <= line("$") | exe "normal g'\"" | endif

" Must be called before 'filetype plugin indent on'
call pathogen#infect()
" Auto-set certain options as well as syntax highlighting and indentation
filetype plugin indent on

" Enables :make to compile, or validate, certain filetypes
" (use :cn & :cp to jump between errors)
au FileType xml,xslt compiler xmllint
au FileType html compiler tidy
au FileType java compiler javac

" For standards-compliant :TOhtml output
let html_use_css=1
let use_xhtml=1

" Helps if you have to use another editor on the same file
autocmd FileChangedShell *
    \ echohl WarningMsg |
    \ echo "File has been changed outside of vim." |
    \ echohl None

" For the help browser: hit enter to activate links, and ctrl-[ as a back button
au FileType help nmap <buffer> <Return> <C-]>
au FileType help nmap <buffer> <C-[> <C-O>

" Matchit now ships with Vim!
runtime! macros/matchit.vim


" Mappings for the ToggleComment Plugin
noremap <silent> ,# :call CommentLineToEnd('# ')<CR>+
noremap <silent> ,/ :call CommentLineToEnd('// ')<CR>+
noremap <silent> ," :call CommentLineToEnd('" ')<CR>+
noremap <silent> ,; :call CommentLineToEnd('; ')<CR>+
noremap <silent> ,- :call CommentLineToEnd('-- ')<CR>+
noremap <silent> ,* :call CommentLinePincer('/* ', ' */')<CR>+
noremap <silent> ,< :call CommentLinePincer('<!-- ', ' -->')<CR>+

" Centers, left, or right-justifies text
"noremap <silent> ,c :ce <CR> << <CR>
"noremap <silent> ,l :le <CR>
"noremap <silent> ,r :ri <CR>

" Sets the default encoding to utf-8 if Vim was compiled with multibyte
"if has("multi_byte")
"    set encoding=utf-8
"    if $TERM == "linux" || $TERM_PROGRAM == "GLterm"
"        set termencoding=latin1
"    endif
"    if $TERM == "xterm" || $TERM == "xterm-color"
"        let propv = system("xprop -id $WINDOWID -f WM_LOCALE_NAME 8s ' $0' -notype WM_LOCALE_NAME")
"        if propv !~ "WM_LOCALE_NAME .*UTF.*8"
"            set termencoding=latin1
"        endif
"    endif
"endif

" }}}

imap jj <esc>
" Set remap space and backspace for quicker movement
" Space already mapped to folding, disable 
"nmap <space> 10j
"nmap <backspace> 10k
"
" Remap : for easier command access
nnoremap ; :

"noremap k j
"noremap j k
" eof
autocmd FileType python set omnifunc=pythoncomplete#Complete

" Toggle spell-checking with ctrl-e in normal mode:
" Need to find a better key for this since C-I is better used for retracing
" movement forward
"nmap <silent> <C-I> :set nospell!<CR>:set nospell?<CR>


" Toggle paste-ing with ctrl-e in insert mode:
set pastetoggle=<C-E>

set path=.,**

"let g:miniBufExplMapWindowNavVim = 1
"let g:miniBufExplMapWindowNavArrows = 1
"let g:miniBufExplMapCTabSwitchBufs = 1
"let g:miniBufExplModSelTarget = 1 

set tags=/ws/steinbek/ws4/rom/tags

"set nocp
filetype plugin on

let mapleader = ","


" Plugin Options

" LustyJuggler
let g:LustyJugglerSuppressRubyWarning = 1

" Fix annoying // Comments interpreted as errors in C files
hi link cCommentError Comment

" EasyMotion mappings:
" Shorten the forwards and backwards find commands
nmap <leader>f <leader><leader>f
nmap <leader>g <leader><leader>F

" Tagbar
nnoremap <silent> <F9> :TagbarToggle<CR>
let g:tagbar_left = 1

" EasyTags
" Use the vim tags location instead of ~/.vimtags
"let g:easytags_dynamic_files = 1
"let g:easytags_file = './tags'

" TagHighlight
if ! exists('gTagHighlightSettings')
   let g:TagHighlightSettings = {}
endif
"let g:TagHighlightSettings['PythonVariantPriority'] = 'python'
let g:TagHighlightSettings['PathToPython'] = '/tools/py'

" Disabled plugins:
set runtimepath-=~/.vim/bundle/taghighlight:~/.vim/bundle/tagbar

" Gvim font
if has("gui_running")
   set guifont=Terminus\ 8
endif

" highlight tabs and trailing spaces
set listchars=tab:>-,trail:-
set list

" Toggle Highlighting
nnoremap <esc> :noh<return><esc>
