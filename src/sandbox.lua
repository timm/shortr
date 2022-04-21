t={10,20,"?",30,405,"?",10}

function lt(a,b) 
  a=a=="?" and -1000000  or a
  b=b=="?" and -1000000  or b
  return a < b end

table.sort(t,lt)
for k,v in pairs(t) do print (k,v) end
