local it = {}
function it.add(i,x,n)  
  if x~="?" then n=n or 1; i.n=i.n+n; i._is.add(i,x,n) end; return x end

function it.bin(  i,... ) return i._is.bin(i,...) end
function it.dist( i,x,y ) return x=="?" and y=="?" and 1 or i._is.dist(i,x,y) end
function it.div(  i,... ) return i._is.div(i,...) end
function it.like( i,... ) return i._is.like(i,...) end
function it.mid(  i,... ) return i._is.mid(i,...) end
function it.ok(   i,... ) return i._is.ok(i,...) end
function it.say(  i,... ) return i._is.say(i,...) end

return it
