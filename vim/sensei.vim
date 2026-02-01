setlocal makeprg=seito

" GHC
setlocal errorformat=%A%f:%l:%c:\ %t%*[^:]:\ [GHC-%n]
setlocal errorformat^=%A%f:%l:%c:\ %t%*[^:]: " GHC 9.6

" lines that start with a space continue the previous message
setlocal errorformat^=%+C\ %.%#

" empty lines terminate a message
setlocal errorformat^=%Z

" ignore this part of the message
setlocal errorformat^=%-G\ \ \ \ Suggested\ fix:%.%#
setlocal errorformat^=%-G\ \ \ \ \ \ Perhaps\ you\ meant\ %.%# " GHC 9.2

" single-line error message
setlocal errorformat+=%f:%l:%c:\ %t%*[^:]:\ [GHC-%n]\ %m
setlocal errorformat+=%f:%l:%c:\ %t%*[^:]:\ %m " GHC 9.6

" Hspec
setlocal errorformat^=\ \ %f:%l:%c:\ .%#
