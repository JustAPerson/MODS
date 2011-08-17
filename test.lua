-- used for development
local m = require("Current.main");

local bytecode = m.Assemble(io.open("Current/example.lasm"):read("*all"));
local f, e = loadstring(bytecode);
print(f, e);
--f()

print(require("Legacy.main").Debug(string.dump(function()
-- used for development
local m = require("Current.main");

local bytecode = m.Assemble(io.open("Current/example.lasm"):read("*all"));
local f, e = loadstring(bytecode);
print(f, e);
--f()

end), true))