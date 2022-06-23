-----
-- Naive Bayes Classifier
local _=require"about"
local obj,push,the = _.obj,_.push,_.the

local NB = obj("NB", function (i,src,report)
  i.overall, i.dict, i.list = nil, {},{}
  report = report or print
  Data.ROWS(src, function(row)
    if not i.overall then i.overall = ROWS(row)  else -- (0) eat row1
      row = i.overall:summarize(row)  -- XX add to overall
      if #i.overall.rows > the.wait then report(Row.klass(row), NB.guess(i,row)) end
      NB.train(i,row) end end)              -- add tp rows's klass
  end)

function NB.train(i,row)
  local kl = row:klass()
  i.dict[kl]     = i.dict[kl] or push(i.list,i.overall.clone()) --klass is known
  i.dict[kl].txt = kl                     -- each klass knows its name
  i.dict[k]:add(row) end                  -- update klass with row

function NB.keymax(i,row)
  most,out = -1, nil
  for key,rows in pairs(i.dict) do
    tmp = rows:like(row,#i.list,#i.overall.rows)
    if tmp > most then most,out = tmp,key end end
  return key end

return NB
