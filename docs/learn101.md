<a name=top>&nbsp;<br>
<img align=left width=175 src="/docs/head.png"> <img 
src="https://img.shields.io/badge/purpose-se,ai-informational?style=flat&logo=hyper&logoColor=white&color=red"> <img 
src="https://img.shields.io/badge/language-lua-informational?style=flat&logo=lua&logoColor=white&color=yellow"> <img 
src="https://img.shields.io/badge/platform-osx,linux-informational?style=flat&logo=linux&logoColor=white&color=orange"> <a
href="https://github.com/timm/l5/actions/workflows/tests.yml"><img src="https://github.com/timm/l5/actions/workflows/tests.yml/badge.svg"></a> <a 
href="https://zenodo.org/badge/latestdoi/206205826"> <img src="https://zenodo.org/badge/206205826.svg" alt="DOI"></a><br>
<b> <a href="https://github.com/timm/l5/blob/master/LICENSE.md">&copy;2022</a> </b>, Tim Menzies, <timm@ieee.org>
<h1>The Little LUA Learning Library</h1><br clear=all>



```lua
local NB=class("NB",OBJ)
function NB:new(data, this) 
  self.n, self.nh, self.wait         = 0,0, the.wait
  self.e, self.h, self.log,self.cols = {},{},{},nil
  for row in items(data) do 
    if   not self.cols
    then self.cols= collect(row,function(j,s) return {name=s,indep=j~=#row} end)
    else self:test(row); self:train(row) end end  end
```



```lua
function NB:test(row)
  if self.n > the.wait then 
    push(self.log,{want=row[#row], got=self:classify(row)}) end end
```



```lua
function NB:train(row)
  local more, kl = false, row[#row]
  for col,x in pairs(row) do 
    if x ~="?" then 
      more = true
      inc3(self.e, col, x, kl)  end end
  if more then
    self.n = self.n + 1
    if not self.h[kl] then self.nh = self.nh + 1 end
    inc(self.h, kl) end end
```



```lua
function NB:classify(t,use)
  local hi,out = -math.huge
  for h,val in pairs(self.h) do 
    local prior = ((self.h[h] or 0) + the.K)/(self.n + the.K*self.nh)
    local l = math.log(prior)
    for col,x in pairs(t) do
      if x ~= "?" and self.cols[col].indep then
        l = l + math.log((has3(self.e,col,x,h) + the.M*prior) /
                         ((self.h[h] or 0) + the.M)) end end 
    if l>hi then hi,out=l,h end end
  return out end
```



```lua
function NB:score()
  local a,n = 0,#self.log
  for key,x in pairs(self.log) do if x.want==x.got then a=a+1/n end end
  return acc,self.log end 
```



```lua
return NB
