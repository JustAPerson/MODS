local mods = require("current");
local bytecode = mods.assemble(io.open("example.lasm", "r"):read("*all"));

local old = string.dump(loadfile("../src/current.lua"));
local new = mods.link(mods.delink(old));

print(#old, #new)
print(os.clock());
