local _={}

_.num    = function(x) return x:find"^[A-Z]" end
_.goal   = function(x) return x:find"[-+!]"  end
_.klass  = function(x) return x:find"!$"     end
_.ignore = function(x) return x:find":$"     end
_.weight = function(x) return x:find"-$" and -1 or 1 end
_.xnum   = function(x) return _.num(x) and not _.goal(x) end

return _
