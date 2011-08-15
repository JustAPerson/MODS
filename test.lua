local mods = require('Legacy.main');
local function test()
	function isPrime(n)
		if n > 2 then
			for i = 2, math.floor(n^0.5) do
				if n % i == 0 then
					return false;
				end
			end
		else
			return n == 2;
		end

		return true;
	end
end

local function test()
	function f()
return 1, 2
end

print(f(), 3)
end


local bytecode = string.dump(test);

print(mods.Debug(bytecode, true));

local code = mods.ParseLASM([[
.cname  '@C:/Users/Jason/Documents/Programming/Lua/MODS/test.lua'
.optns 0 0 0 4

.const 'f'			;	k0
.const 'print'			;	k1
.const 3			;	k2

.funct	;	number 0
	.cname  ''
	.optns 0 0 0 2

	.const 1			;	k0
	.const 2			;	k1

	LOADK 0 k0
	LOADK 1 k1
	RETURN 0 3 0
	RETURN 0 1 0
.end

CLOSURE 0 0
SETGLOBAL 0 k0
GETGLOBAL 0 k1
GETGLOBAL 1 k0
CALL 1 1 3
LOADK 3 k2
CALL 0 4 1
RETURN 0 1 0
]]);

local a = assert(loadstring(code));
print(pcall(a));

print(code:gsub(".", function(a) return "\\" .. a:byte() end))