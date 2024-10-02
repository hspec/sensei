set makeprg=seito

" GHC
set errorformat=%A%f:%l:%c:\ %t%*[^:]:\ [GHC-%n]
set errorformat^=%A%f:%l:%c:\ %t%*[^:]: " GHC 9.6

" lines that start with a space continue the previous message
set errorformat^=%+C\ %.%#

" empty lines terminate a message
set errorformat^=%Z

" ignore this part of the message
set errorformat^=%-G\ \ \ \ Suggested\ fix:%.%#
set errorformat^=%-G\ \ \ \ \ \ Perhaps\ you\ meant\ %.%# " GHC 9.2

" single-line error message
set errorformat+=%f:%l:%c:\ %t%*[^:]:\ [GHC-%n]\ %m
set errorformat+=%f:%l:%c:\ %t%*[^:]:\ %m " GHC 9.6

" Hspec
set errorformat^=\ \ %f:%l:%c:\ .%#
