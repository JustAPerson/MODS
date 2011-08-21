local mods = require("CURRENT");
local bytecode = mods.Assemble(io.open("example.lasm", "r"):read("*all"));

local old = string.dump(loadfile("../src/current.lua"));
local new = mods.Link(mods.Delink(old));

print(#old, #new)
print(os.clock());