In each language-specific vimrc, I have one of these:

vnoremap <buffer> ,c :!cmt --<cr>

In the haskell vimrc:

vnoremap <buffer> ,t :!string-literal --explicit-nl --toggle-backslash<cr>
vnoremap <buffer> ,T :!string-literal --toggle-lines<cr>
