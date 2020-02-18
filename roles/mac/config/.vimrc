if has('mouse')
  set mouse=a
endif

filetype off
filetype plugin indent on
:syntax on
set laststatus=2
set background=dark
set nocompatible
"set backspace=2
set backspace=indent,eol,start
set encoding=utf-8
set fileencoding=utf-8
set wrap linebreak nolist
set cursorline

set ttimeoutlen=10 "Понижаем задержку ввода escape последовательностей
let &t_SI.="\e[5 q" "SI = режим вставки
let &t_SR.="\e[3 q" "SR = режим замены
let &t_EI.="\e[1 q" "EI = нормальный режим

call pathogen#helptags()
execute pathogen#infect()
let g:solarized_termcolors=256
colorscheme solarized

:set runtimepath^=~/.vim/bundle/node
:set runtimepath^=~/.vim/bundle/ctrlp.vim
:set runtimepath^=~/.vim/bundle/vim-javascript
:set runtimepath^=~/.vim/bundle/vim-fugitive
:set runtimepath^=~/.vim/bundle/vim-ingo-library
:set runtimepath^=~/.vim/bundle/vim-SyntaxRange
:set runtimepath^=~/.vim/bundle/vim-markdown

au BufNewFile,BufRead *.yaml,*.yml so ~/.vim/plugged/vim-yaml/after/syntax/yaml.vim

let g:lightline = {
      \ 'active': {
      \   'left': [ [ 'mode', 'paste' ],
      \             [ 'readonly', 'filename', 'gitbranch', 'modified','charvaluehex'] ]
      \ },
      \ 'component_function': {
      \   'gitbranch': 'fugitive#head'
      \ },
      \ 'component': {
      \   'charvaluehex': '0x%B'
      \ },
      \ }

let g:EditorConfig_core_mode = 'external_command'

""" *********** """
""" MAPPINGS    """
""" *********** """

" Markdown
nmap <C-s> <Plug>MarkdownPreview
nmap <M-s> <Plug>MarkdownPreviewStop
nmap <C-p> <Plug>MarkdownPreviewToggle

" Quick Find Window
nnoremap ‘ :cn<CR>
nnoremap “ :cp<CR>

nmap T :tabnew<CR>
nmap ’ :tabnext<CR>
nmap ” :tabprevious<CR>

nmap <C-p> :CtrlP<CR>

:map! <M-BS> <C-W>

" go
nmap F :GoFmt<CR>
nmap ˆ :GoImports<CR>
nmap gr :GoRename<CR>
nmap gb :GoBuild<CR>

nnoremap <silent> * :vim <cword> <C-R><C-W> ** <CR>

""" ******* """
""" Airline """
""" ******* """
"let g:airline_powerline_fonts = 1
"let g:airline#extensions#keymap#enabled = 0
"let g:airline_section_z = "\ue0a1:%l/%L Col:%c"
"let g:Powerline_symbols='unicode'
"let g:airline#extensions#xkblayout#enabled = 0
"set guifont=Fura\ Code\ Light\ Nerd\ Font\ Complete:h16

""" ************** """
""" Plugin Install """
""" ************** """

" Specify a directory for plugins
" - For Neovim: stdpath('data') . '/plugged'
" - Avoid using standard Vim directory names like 'plugin'
call plug#begin('~/.vim/plugged')
Plug 'vim-airline/vim-airline'
Plug 'junegunn/vim-easy-align'
Plug 'https://github.com/junegunn/vim-github-dashboard.git'
Plug 'scrooloose/nerdtree', { 'on':  'NERDTreeToggle' }
Plug 'tpope/vim-fireplace', { 'for': 'clojure' }
Plug 'rdnetto/YCM-Generator', { 'branch': 'stable' }
Plug 'fatih/vim-go', { 'tag': '*' }
Plug 'nsf/gocode', { 'tag': 'v.20150303', 'rtp': 'vim' }
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
" Unmanaged plugin (manually installed and updated)
Plug '~/my-prototype-plugin'
Plug 'https://github.com/iamcco/markdown-preview.nvim.git'
Plug 'https://github.com/romainl/vim-qf.git'
Plug 'https://github.com/martinda/Jenkinsfile-vim-syntax.git'
Plug 'ctrlpvim/ctrlp.vim'
Plug 'kien/tabman.vim'
Plug 'davidhalter/jedi-vim'
Plug 'https://github.com/pearofducks/ansible-vim.git'
" Java
Plug 'artur-shaik/vim-javacomplete2'
Plug 'tpope/vim-fugitive'
Plug 'stephpy/vim-yaml'
Plug 'editorconfig/editorconfig-vim'
Plug 'sheerun/vim-polyglot'
"Plug 'jupyter-vim/jupyter-vim'
Plug 'jmcantrell/vim-virtualenv'
"""Plug 'Valloric/YouCompleteMe'
Plug 'neomake/neomake'

" Initialize plugin system
call plug#end()
" call mkdp#util#install()

""" ***************** """
""" Markdown-preview  """
""" ***************** """
set nofoldenable
" set to 1, nvim will open the preview window after entering the markdown buffer
" default: 0
let g:mkdp_auto_start = 0
" set to 1, the nvim will auto close current preview window when change
" from markdown buffer to another buffer
" default: 1
let g:mkdp_auto_close = 1
" set to 1, the vim will refresh markdown when save the buffer or
" leave from insert mode, default 0 is auto refresh markdown as you edit or
" move the cursor
" default: 0
let g:mkdp_refresh_slow = 0
" set to 1, the MarkdownPreview command can be use for all files,
" by default it can be use in markdown file
" default: 0
let g:mkdp_command_for_global = 0
" set to 1, preview server available to others in your network
" by default, the server listens on localhost (127.0.0.1)
" default: 0
let g:mkdp_open_to_the_world = 0
" use custom IP to open preview page
" useful when you work in remote vim and preview on local browser
" more detail see: https://github.com/iamcco/markdown-preview.nvim/pull/9
" default empty
let g:mkdp_open_ip = ''
" specify browser to open preview page
" default: ''
let g:mkdp_browser = ''
" set to 1, echo preview page url in command line when open preview page
" default is 0
let g:mkdp_echo_preview_url = 0
" a custom vim function name to open preview page
" this function will receive url as param
" default is empty
let g:mkdp_browserfunc = ''
" options for markdown render
" mkit: markdown-it options for render
" katex: katex options for math
" uml: markdown-it-plantuml options
" maid: mermaid options
" disable_sync_scroll: if disable sync scroll, default 0
" sync_scroll_type: 'middle', 'top' or 'relative', default value is 'middle'
"   middle: mean the cursor position alway show at the middle of the preview page
"   top: mean the vim top viewport alway show at the top of the preview page
"   relative: mean the cursor position alway show at the relative positon of the preview page
" hide_yaml_meta: if hide yaml metadata, default is 1
" sequence_diagrams: js-sequence-diagrams options
let g:mkdp_preview_options = {
    \ 'mkit': {},
    \ 'katex': {},
    \ 'uml': {},
    \ 'maid': {},
    \ 'disable_sync_scroll': 0,
    \ 'sync_scroll_type': 'middle',
    \ 'hide_yaml_meta': 1,
    \ 'sequence_diagrams': {}
    \ }
" use a custom markdown style must be absolute path
let g:mkdp_markdown_css = ''
" use a custom highlight style must absolute path
let g:mkdp_highlight_css = ''
" use a custom port to start server or random for empty
let g:mkdp_port = ''
" preview page title
" ${name} will be replace with the file name
let g:mkdp_page_title = '「${name}」'

""" ************ """

" yaml stuffs
au! BufNewFile,BufReadPost *.{yaml,yml} set filetype=yaml
autocmd FileType yaml setlocal ts=2 sts=2 sw=2 expandtab
