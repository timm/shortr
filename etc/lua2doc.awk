function trim(x) {sub(/^[ \t]+/,"",x); sub(/[ \t]+$/,"",x); return x }

sub(/^------------ /,"-- ## ")   { print $0"\n\n" ; next}    
sub(/^--------- /,   "-- ### ")  { print $0"\n\n" ; next}     
sub(/^------ /,      "-- #### ") { print $0"\n\n" ; next}      
sub(/^--- /,         "")         { split($0,h,/ -- /)                                 
                                   h[1] = gensub(/([A-Za-z0-9_]+):/," `\\1`:  ","g",h[1])
                                   print "--**"trim(h[1])"**<br>"trim(h[2]) ; next
                                 }      
1
