#!/bin/bash
while true; do
  echo
  ./vim/test/run.vim
  inotifywait -e modify -e attrib -e close_write -e move -e create -e delete -r vim/
done
