#!/bin/env -S vim -u NONE -S

set t_ti=
set t_te=

highlight red ctermfg=red
highlight green ctermfg=green

command -nargs=* FAILURE echohl red | echo <args> | echohl none
command -nargs=* SUCCESS echohl green | echo <args> | echohl none

function ShouldBe(actual, expected)
  if !(a:actual ==# a:expected)
    throw "expected: " . a:expected . "\n but got: " . a:actual
  endif
endfunction

try
  for name in glob("**/*.test.vim", v:true, v:true)
    SUCCESS "running " . name
    execute "source " . name
  endfor
catch
  FAILURE substitute(substitute(v:throwpoint, '^command line..script ', '', ''), expand('<script>') . '\v\[\d+]\.\.(.*)\[(\d+)\]\.\.function ShouldBe, line \d+', '\1:\2', '')
  echo "\n"
  FAILURE v:exception
  cquit
endtry

SUCCESS "\nSUCCESS"
quit
