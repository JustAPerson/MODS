local mods = require "src.current"
local test = loadfile "tests/current/strip_example.lua"

local old = string.dump(test)
local new = mods.strip(old)
local expected

assert(loadstring(old)() == 18)
assert(loadstring(new)() == 18)

do
	os.execute("luac -s tests/current/strip_example.lua")
	local file = io.open("luac.out", "r")
	expected = file:read("*all")
	file:close()
	os.remove("luac.out")
end

local function hex(s)
	return s:gsub(".", function(c) return ("%02X"):format(c:byte()) end)
end

if new == expected then
	print("Success")
else
	print("Failure")
	print(("Length mismatch. Expected: %08X. Got: %08X"):format(#expected,
	                                                            #new))
	local wrong = 0
	for i = 1, math.max(#expected, #new) do
		local n, e = new:sub(i,i), expected:sub(i,i)
		if n ~= e then
			wrong = wrong + 1
			print(("%08X: Expected: %s. Got: %s"):
			      format(i, e ~= "" and hex(e) or "..", n ~= "" and hex(n) or ".."))
		end
	end
	print(("Wrong: %08X"):format(wrong))
end