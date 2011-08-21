local mods = require("CURRENT");
local bytecode = mods.Assemble(io.open("example.lasm", "r"):read("*all"));


loadstring(bytecode)()