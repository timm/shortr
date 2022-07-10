function cat(t) 
  u={}; for k,v in pairs(t) do u[1+#u]=(":%s %s"):format(k,v) end 
  table.sort(u)
  return table.concat(u," ") end

function class(base)
    local function new(kl,...)
        local self = setmetatable({super=base},kl)
        kl._init(self,...)
        return self
    end
    local klass, base_ctor = {__tostring=cat,__call=new}
    klass.__index = klass
    if base then
        setmetatable(klass,base)
        klass._base = base
        base_ctor = rawget(base,'_init')
    end
    return klass
end



Animal = class()

function Animal:_init (name)
    self.name = name.."s"
end

function Animal:kind ()
    return 'unknown!'
end

function Animal:__tostring ()
    return "animal "..self.name
end

print(Animal("tiger"))

Cat = class(Animal)

--function Cat:_init (name) self.super._init(self,name.."!!") end

function Cat:kind ()
    return 'cat'
end

--~ function Cat:__tostring ()
--~     return "meeoww "..self.name
--~ end

felix = Cat('felix')
print(felix)
