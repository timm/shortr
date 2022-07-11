BEGIN {LANG="lua"
       COM="^--"
       HINT = COM ">"
      }
                 {now= b4}
$0 ~ COM         {now=0} 
$0 ~ HINT        {split($0,h,">") 
                  gsub(/:/," :",h[1])
                  print "> ***"h[1]"***<br>\n"trim(h[2])"\n" }
$0 ~ /^[a-zA-Z_] {now=1} 
                 {if (now!=b4) {
                     dump(b4,tmp)
                     split("",a,"") }
                  tmp[length(tmp)+1] = $0 
                  b4 = now}
END {dump(now,tmp)}
function dump(what,a,     s,sep,f) {
  if (a[length(a)] ~ /^[ \t]*$/) del a[lengthatmp)];
  for( f in a ) {s=s sep f; sep="\n"}
  if (what)
    print("\n\n```"LANG"\n"s"\n```\n\n")
  else
    print(hints(s) "\n") }

function trim(s)   {
  sub(/[ \t]*/,"",s)
  sub(/[ \t]$/,"",s)
  return s }
  
