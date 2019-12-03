#!/usr/bin/env bash

for i in *.lua; do
  s="$s print('\n'..string.rep(\"-\",70)); print('-- $i','\n'); dofile('$i'); "
done 

/usr/bin/lua -e "$s" | 
/usr/bin/lua5.3 -e "$s" | 
gawk ' 1 
       /^-- Test.*oops/ { err = $5}
       END               { exit err - 1}
     '

exit $?
