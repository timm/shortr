#!/usr/bin/env bash
for i in "$@"; do
  if [[ "$i" = "eof" ]]
  then  echo -n "" 
  else 
     figlet -W -fsmall  ${i%%.*}  | 
     gawk 'BEGIN       { "-------------------------------------------------------------------"} \
           /^[  \t]*$/ { next } 
                       {print "---    "$0} 
           END         { print""}'
     cat $i
  fi
done
