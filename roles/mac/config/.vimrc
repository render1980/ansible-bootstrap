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

call plug#begin('~/.vim/plugged')
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

""" INCLUDES """
""" ******** """
source ~/.vimrc-keymap
source ~/.vimrc-plug
if has('nvim')
  source ~/.vimrc-coc
else
  source ~/.vimrc-python
endif
source ~/.vimrc-md
source ~/.vimrc-xml
source ~/.vimrc-lint
