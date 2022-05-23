-- vim: ts=2 sw=2 et : 
-- LOOK.LUA: landscape analysis 
-- (c) 2022 Tim Menzies, timm@ieee.org, BSD-2 license 
local l,L  = require"lib", require"look"
local any,cli,csv,main,map = l.any, l.cli, l.csv, l.main, l.map
local o, oo,shuffle,sort   = l.o, l.oo, l.shuffle, l.sort
local ROW,ROWS             = L.ROW, L.ROWS
local the                  = cli(L.the,L.help)
--------------------------------------------------------------------------------
local go,no={},{} -- place to store enabled and disabled tests

function go.the() 
  if the.loud then oo(the) end; return type(the.seed)=="number" end

function go.row(    n) 
  n=0
  for r in csv(the.file) do n=n+#r; if the.loud then oo(r) end end 
  return n == 3192 end

function go.egs(    rows) 
  rows= ROWS(the.file)
  if the.loud then map(rows.nums,oo) end
  return rows.nums[1].hi==8 end

function go.clone(    rows) 
  rows= ROWS(the.file)
  oo(rows:mid()) end

function go.dist(    r1,rows,ok) 
  ok,rows= true, ROWS(the.file); 
  r1 = rows.rows[1]
  for _,r2 in pairs(rows.rows) do 
    ok = ok and r2:dist(r2)==0 
    ok = ok and r1:dist(r2) == r2:dist(r1) end 
  return ok end 

function go.around(    r1,rows, order) 
  rows = ROWS(the.file); 
  r1 = rows.rows[1]
  order = rows:around(r1)
  return order[#order//3].dist < order[#order//2].dist  end

function go.far(    rows,r1,r2,ok)
  ok = true
  rows = ROWS(the.file); 
  for k=1,50 do
    r1 = rows:far(any(rows.rows))
    r2 = rows:far(r1) 
    ok = ok and r1:dist(r2) > .5 end 
  return ok end

function go.betters(  t,n1) 
  t=sort(ROWS(the.file).rows)
  n1=10
  for k =1,n1 do oo(t[k].cells) end; print""
  for k =#t-n1, #t do oo(t[k].cells) end
  return t[1] < t[#t]
end

function go.look(   rs,best,bests,rests,n)
  for i=1,20 do 
    print("")
    rs = ROWS(the.file)
    rs.rows = shuffle(rows.rows)
    best,bests,rests = rs:look() 
    for n,r in pairs(sort(rs.rows)) do r.rank = n // (#rows.rows // (6/.35)) end
    for _,r in pairs(bests) do print(r.rank) end
    n=0
    for _,r in pairs(rs.rows) do if r.evaluated then n=n+1 end end
    oo{bests=#bests,rests=#rests,n=n} end
  return true end
--------------------------------------------------------------------------------
main(go, the)
