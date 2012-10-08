local mods = require "src.current"
local function test()
	local x = 5
	local function change()
		x = x + 2
	end
	change()
	return x
end


local expected = string.dump(test)
local new = mods.assemble(mods.disassemble(test))

local function hex(s)
	return s:gsub(".", function(c) return ("%02X"):format(c:byte()) end)
end

if new == expected then
	print("Success")
else
	print("Failure")
	print(("Length Expected: %08X. Got: %08X"):format(#expected,
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