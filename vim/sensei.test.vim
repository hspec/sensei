source vim/sensei.vim

function Require(actual, required)
  for i in range(len(a:required))
    if a:actual[i] > a:required[i]
      return 1
    elseif a:actual[i] < a:required[i]
      return 0
    endif
  endfor
  return 1
endfunction

function GhcVersion(name)
  return matchlist(a:name, '\vghc-(\d+)\.(\d+)\.(\d+)\.errors')[1:3]
endfunction

function PopulateQuickFixList(name)
  SUCCESS a:name
  execute "cgetfile " . a:name
  return filter(getqflist(), 'v:val.valid')
endfunction

function GhcErrorsFor(name)
  let errors = glob("vim/test/fixtures/" . a:name . ".*.errors", v:true, v:true)
  call ShouldBe(len(errors), 5)
  return errors
endfunction

for name in GhcErrorsFor("lexical-error.hs")
  let errors = PopulateQuickFixList(name)
  call ShouldBe(len(errors), 1)

  let err = errors[0]
  call ShouldBe(bufname(err.bufnr), "lexical-error.hs")
  call ShouldBe(err.lnum, 1)
  call ShouldBe(err.col, 11)
  call ShouldBe(err.end_lnum, 0)
  call ShouldBe(err.end_col, 0)
  call ShouldBe(err.type, 'e')
  if Require(GhcVersion(name), [9,6])
    call ShouldBe(err.nr, 21231)
  else
    call ShouldBe(err.nr, -1)
  endif
  call ShouldBe(err.text, "\n    lexical error in string/character literal at character '\\n'")
endfor

for name in GhcErrorsFor("parse-error.hs")
  let errors = PopulateQuickFixList(name)
  call ShouldBe(len(errors), 1)

  let err = errors[0]
  call ShouldBe(bufname(err.bufnr), "parse-error.hs")
  call ShouldBe(err.lnum, 1)
  call ShouldBe(err.col, 1)
  call ShouldBe(err.end_lnum, 0)
  call ShouldBe(err.end_col, 0)
  call ShouldBe(err.type, 'e')
  if Require(GhcVersion(name), [9,8])
    call ShouldBe(err.nr, 25277)
  else
    call ShouldBe(err.nr, -1)
  endif
  call ShouldBe(err.text, "\n    Parse error: module header, import declaration\n    or top-level declaration expected.")
endfor

for name in GhcErrorsFor("type-error.hs")
  let errors = PopulateQuickFixList(name)
  call ShouldBe(len(errors), 1)

  let err = errors[0]
  call ShouldBe(bufname(err.bufnr), "type-error.hs")
  call ShouldBe(err.lnum, 2)
  call ShouldBe(err.col, 7)
  call ShouldBe(err.end_lnum, 0)
  call ShouldBe(err.end_col, 0)
  call ShouldBe(err.type, 'e')
  if Require(GhcVersion(name), [9,6])
    call ShouldBe(err.nr, 83865)
    call ShouldBe(err.text, "\n    Couldn't match type ‘Int’ with ‘[Char]’\n    Expected: String\n      Actual: Int")
  else
    call ShouldBe(err.nr, -1)
    call ShouldBe(err.text, "\n    • Couldn't match type ‘Int’ with ‘[Char]’\n      Expected: String\n        Actual: Int\n    • In the expression: 23 :: Int\n      In an equation for ‘foo’: foo = 23 :: Int")
  endif
endfor

for name in GhcErrorsFor("suggested-fix.hs")
  let errors = PopulateQuickFixList(name)
  call ShouldBe(len(errors), 2)

  let err = errors[0]
  call ShouldBe(bufname(err.bufnr), "suggested-fix.hs")
  call ShouldBe(err.lnum, 1)
  call ShouldBe(err.col, 1)
  call ShouldBe(err.end_lnum, 0)
  call ShouldBe(err.end_col, 0)
  call ShouldBe(err.type, 'e')
  if Require(GhcVersion(name), [9,6])
    call ShouldBe(err.nr, 44432)
  else
    call ShouldBe(err.nr, -1)
  endif
  call ShouldBe(err.text, "\n    The type signature for ‘foo’ lacks an accompanying binding")

  let err = errors[1]
  call ShouldBe(bufname(err.bufnr), "suggested-fix.hs")
  call ShouldBe(err.lnum, 4)
  call ShouldBe(err.col, 1)
  call ShouldBe(err.end_lnum, 0)
  call ShouldBe(err.end_col, 0)
  call ShouldBe(err.type, 'e')
  if Require(GhcVersion(name), [9,6])
    call ShouldBe(err.nr, 44432)
  else
    call ShouldBe(err.nr, -1)
  endif
  call ShouldBe(err.text, "\n    The type signature for ‘bar’ lacks an accompanying binding")
endfor

for name in GhcErrorsFor("suggested-fix-multiline.hs")
  let errors = PopulateQuickFixList(name)
  call ShouldBe(len(errors), 2)

  let err = errors[0]
  call ShouldBe(bufname(err.bufnr), "suggested-fix-multiline.hs")
  call ShouldBe(err.lnum, 1)
  call ShouldBe(err.col, 1)
  call ShouldBe(err.end_lnum, 0)
  call ShouldBe(err.end_col, 0)
  call ShouldBe(err.type, 'e')
  if Require(GhcVersion(name), [9,6])
    call ShouldBe(err.nr, 44432)
  else
    call ShouldBe(err.nr, -1)
  endif
  call ShouldBe(err.text, "\n    The type signature for ‘foo’ lacks an accompanying binding")

  let err = errors[1]
  call ShouldBe(bufname(err.bufnr), "suggested-fix-multiline.hs")
  call ShouldBe(err.lnum, 4)
  call ShouldBe(err.col, 1)
  call ShouldBe(err.end_lnum, 0)
  call ShouldBe(err.end_col, 0)
  call ShouldBe(err.type, 'e')
  if Require(GhcVersion(name), [9,6])
    call ShouldBe(err.nr, 44432)
  else
    call ShouldBe(err.nr, -1)
  endif
  call ShouldBe(err.text, "\n    The type signature for ‘bar’ lacks an accompanying binding")
endfor

let errors = PopulateQuickFixList("vim/test/fixtures/hspec.hs.errors")
call ShouldBe(len(errors), 1)

let err = errors[0]
call ShouldBe(bufname(err.bufnr), "vim/test/fixtures/hspec.hs")
call ShouldBe(err.lnum, 6)
call ShouldBe(err.col, 11)
call ShouldBe(err.end_lnum, 0)
call ShouldBe(err.end_col, 0)
call ShouldBe(err.type, '')
call ShouldBe(err.nr, -1)
call ShouldBe(err.text, "")

let errors = PopulateQuickFixList("vim/test/fixtures/one-line.errors")
call ShouldBe(len(errors), 1)

let err = errors[0]
call ShouldBe(bufname(err.bufnr), "src/Editor/Vim.hs")
call ShouldBe(err.lnum, 30)
call ShouldBe(err.col, 3)
call ShouldBe(err.end_lnum, 0)
call ShouldBe(err.end_col, 0)
call ShouldBe(err.type, 'e')
call ShouldBe(err.nr, 76037)
call ShouldBe(err.text, "Not in scope: ‘cexpr’")
