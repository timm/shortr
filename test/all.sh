#!/usr/bin/env bash

for i in *.lua; do
  s="$s print('\n'..string.rep(\"-\",70)); print('-- $i','\n'); dofile('$i'); "
done 

../lua-5.3.5/src/lua  -e "$s" | 
gawk ' 
1                                  # a) print current line      
/^-- Test.*oops/ { err = $5}       # b) catch current error number
END               { exit err - 1}  # c) one test is designed to fail 
                                   #    (just to test the test engine)
                                   #    so "pass" really means, "only
                                   #    one test fails"
'

