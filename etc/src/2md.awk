BEGIN {LANG="lua"
       COM="^-- "
       HINT = COM ">"
       split("",tmp,"")
       print "\n|What|Notes|" >> "/dev/stderr"
       print "|----:|------|" >> "/dev/stderr"
}
                  {now=b4}
$0 ~ COM          {now=0} 
$0 ~ HINT         {split($0,h,/[><]/) 
                   h[2]=gensub(/([A-Za-z0-9_]+)[ \t]*:/,"`\\1`:","g",h[2])
                   gsub(/:/," :",h[2])
                   $0= "> ***"trim(h[2])"***<a id="++n"></a><br>\n"trim(h[3])"\n" 
                   print("|[***"trim(h[2])"***](#"n")|"trim(h[3])"|")>>"/dev/stderr"
                  }
$0 ~ /^[a-zA-Z_]/ {now=1} 
                  {if (now!=b4) dump(b4,tmp)
                   sub(COM,"")
                   tmp[length(tmp)+1] = $0 
                   b4 = now}
END               {dump(now,tmp)
                   print "\n" >> "/dev/stderr"}

function dump(what,a,     s,sep,f) {
  for( f in a ) {s=s sep a[f]; sep="\n"}
  print(what ? "\n```"LANG"\n"s"\n```\n" : s)
  split("",a,"") }

function trim(s)   {
  sub(/^[ \t]*/,"",s)
  sub(/[ \t]*$/,"",s)
  return s }
  
