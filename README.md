Some small source code manipulation tools I use with vim.  They're external
filters, though, so if you use acme instead of vim, they should work fine there
too.  See included `vimrc` for examples.

- cmt - Comment and uncomment code.

- string-literal - Toggle between raw text and list of lines or Haskell
backslash string gap style.  Copy paste from its usage:

`string-literal [ --wrapped --{add,remove,toggle}-{backslash,lines} ]`

Convert between plain text and either backslash-continued string
literals, or list of lines style strings.  This is to work around
haskell's lack of multi-line string literals.  Bind the toggle variant
to a vim key to switch between raw text and haskell string literal.

It assumes a single level of indent for the strings, and leaves the
indent even in the raw form, so that the text will continue to fit in
more or less the same number of columns.  This means they will look
a bit short if printed literally on the terminal, but if you care about
that, use --wrapped mode and have some terminal-aware layer do the
wrapping.

--wrapped mode assumes that someone else will be wrapping the text.
It doesn't put in newlines, and separates wrapped with a leading space.
A paragraph newline in the input becomes an explicit newline in the
haskell string.  Since it assumes someone else is wrapping, it won't
preserve your own leading spaces.  If you are doing explicit formatting
then don't use --wrapped.

Standard CPP doesn't like Haskell string-gap syntax.  You can either use
cpphs via -pgmP 'cpphs --cpp', or use lines mode, which is more
cluttered but doesn't make CPP mad.  Presumably you have a unlines or
Text.unlines call at the front of the list.
