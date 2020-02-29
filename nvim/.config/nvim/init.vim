call plug#begin()
    Plug 'icymind/NeoSolarized'
    Plug 'scrooloose/nerdtree'
    Plug 'rust-lang/rust.vim'
    Plug 'lervag/vimtex'
    Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
    Plug 'autozimu/LanguageClient-neovim', { 'branch': 'next', 'do': 'bash install.sh' }
call plug#end()

syntax on
set backspace=indent,eol,start
set number
set hidden
set history=100
set expandtab
set tabstop=4
set shiftwidth=4
set visualbell

set termguicolors

noremap <C-J> <C-W><C-J>
noremap <C-K> <C-W><C-K>
noremap <C-L> <C-W><C-L>
noremap <C-H> <C-W><C-H>

colorscheme NeoSolarized
set background=dark

"set cmdheight=2
set updatetime=300

autocmd BufNewFile,BufRead *.tera set syntax=html

map <C-n> :NERDTreeToggle<CR>

let g:LanguageClient_serverCommands = {
            \ 'rust': ['~/.cargo/bin/rustup', 'run', 'nightly', 'rls']
            \ }

noremap <silent> K :call LanguageClient#textDocument_hover()<CR>
noremap <silent> gd :call LanguageClient#textDocument_definition()<CR>
noremap <silent> <F2> :call LanguageClient#textDocument_rename()<CR>

let g:rustfmt_autosave = 1

let g:vimtex_view_general_viewer = 'evince'
let g:vimtex_compiler_progname = 'nvr'
let g:vimtex_compiler_method = 'tectonic'

let g:deoplete#enable_at_startup = 1
