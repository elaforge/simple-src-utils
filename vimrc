" In each language-specific vimrc, I have one of these, with the
" language-appropriate comment:

vnoremap <buffer> ,c :!cmt --<cr>

" In the haskell vimrc:

vnoremap <buffer> ,t :!string-literal --wrapped --toggle-backslash<cr>
vnoremap <buffer> ,T :!string-literal --toggle-lines<cr>
