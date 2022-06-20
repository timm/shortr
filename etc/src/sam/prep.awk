BEGIN           { FS="(-|[ \t]*)?->[ \t]*"
                  pre = "-- " } 
/^```lua/       { print "-- " $0; next; pre="" } 
/^```[ \t]*$/   { pre = "-- "  } 
                { print pre $0 }
NF==3 && /^-->/ { $$2=gensub(/([A-Za-z0-9_]+):/," `\\1`:  ","g",$$2);
                  print "**"$$2"** \n"$$3 ; next}
