local {the={rations-260}

local _=require"etc"
local cat,cli,coerce,csv,klass = _.cat, _.cli, _.coerce, _.csv, _.klass
local lines,push,rogues, words = _.lines, _.push, _.rogues, _.words

local Cols, Num, Sym = klass"Cols", klass"Num", klass"Sym"

function Num:new(at,txt) 
  txt = txt or ""
  return {n=0,at=at or 0, txt=txt, ready=false, has={},
          w=txt:find"-$" and -1 or 1} end

function Sym:new(at,txt) 
  return {n=0,at=at or 0, txt=txt or "", ready=false, has={}} end

function Cols:new(names)
  self.names, self.x, self.y, self.all= names, {}, {}, {} 
  for at,txt in pairs(names) do
    local what = txt:find"^[A-Z]" and Num or Sym
    local col  = push(self.all, what(at,txt))
    if not txt:find":$" then
      push(txt:find"[!+-]$" and self.y or self.x, col) end end end

local function rows(src)
  if type(src) == "table" then return s end
  local t={}
  csv(src, function(line) push(t,line) end)
  return t end

function Data:new(src)
  src = 
 

function Data:new(rows)
  for i,row in pairs(rows) do
    if i==1 
    then i.cols = Cols(row) 
    else for cols in pairs{i.cols.x, i.cols.y} do
           for col in pairs(cols) do col:add(row[col.at]) end end end end 

function NUM:add(x)
  if x ~= "?" then
    local pos
    self.n = self.n + 1
    if     #self.has < the.ratios        then pos = 1 + (#self.has) 
    elseif rand()    < the.ratios/self.n then pos = rand(#self.has) end
    if pos then
      self.ready=false -- the `kept` list is no longer in sorted order
      self.has[pos]=x end end end


rogues()
return {Cols, Num, Sym}
