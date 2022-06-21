local the=require"about"
local _=require"lib"
local chat,csv,goes,maps,sort = _.chat, _.csv, _.goes, _.maps, _.sort

-- ## Define test suites
-- To disable a test, rename it from `go` to `no`.
local go,no = {},{}

function go.the()  chat(the); return true end
function go.sort() chat(sort{10,5,1,15,0}); return true end
function go.maps() chat(maps({1,2,3},{10,20,30}, 
                             function(x,y) return x+y end)); return true end

function go.rows() csv(the.file, chat); return true end
  
-- ## Start
goes(the, go)
