local OpSpecs = require "current.opcodes"
local bit = require "current.bit"

local p2 = {1,2,4,8,16,32,64,128,256, 512, 1024, 2048, 4096, 8192, 16384, 32768, 65536, 131072}
local function keep (x, n) return x % p2[n+1] end
local function srb (x,n) return math.floor(x / p2[n+1]) end
local function slb (x,n) return x * p2[n+1] end

local OpcodeEncode = function(op)
	local c0, c1, c2, c3
	if op.OpcodeType == "AsBx" then op.B = op.B + 131071 op.OpcodeType = "ABx" end
	if op.OpcodeType == "ABx" then op.C = keep(op.B, 9); op.B = srb (op.B, 9) end
	c0 = OpSpecs[op.Opcode][1] + slb(keep(op.A, 2), 6)
	c1 = srb(op.A, 2) + slb(keep(op.C, 2), 6)
	c2 = srb(op.C, 2) + slb(keep (op.B, 1), 7)
	c3 = srb(op.B, 1)
	return string.char(c0, c1, c2, c3)
end

local OpcodeChecks = {
	MOVE = function(tab, ins)
		assert(ins.C == 0, "Err: MOVE.C must equal 0")
		assert(ins.A < tab.MaxStackSize, "Err: MOVE.A out of bounds")
		assert(ins.B < tab.MaxStackSize, "Err: MOVE.B out of bounds")
	end,
	LOADK = function(tab, ins)
		assert(ins.A < tab.MaxStackSize, "Err: LOADK.A out of bounds")
		assert(ins.Bx < tab.NumberOfConstants, "Err: LOADK.Bx out of bounds")
	end,
	LOADBOOL = function(tab, ins)
		assert(ins.A < tab.MaxStackSize, "Err: LOADBOOL.A out of bounds");
		assert(ins.B < 2, "Err: LOADBOOL.B invalid value");
		assert(ins.C < 2, "Err: LOADBOOL.C invalid value");
	end,

}
setmetatable(OpcodeChecks, {__index = function() return function() end end})

local DumpBinary
DumpBinary = {
	String = function(s)
		return DumpBinary.SizeT(#s+1)..s.."\0"
	end,
	SpecString = function(s)
		return #s == 0 and DumpBinary.SizeT(0) or DumpBinary.SizeT(#s+1) .. s .. "\0";
	end,
	Int8 = function(n)
		return string.char(n)
	end,
	Int32 = function(x)
		local v = ""
		x = math.floor(x)
		if x >= 0 then
			for i = 1, 4 do
			v = v..string.char(x % 256)
			x = math.floor(x / 256)
			end
		else -- x < 0
			x = -x
			local carry = 1
			for i = 1, 4 do
				local c = 255 - (x % 256) + carry
				if c == 256 then c = 0; carry = 1 else carry = 0 end
				v = v..string.char(c)
				x = math.floor(x / 256)
			end
		end
		return v
	end,
	Int64 = function(x)
		-- FIXME Actual 64 bit encoding.
		return DumpBinary.Int32(x) .. "\0\0\0\0";
	end,
	Float64 = function(x)
		local function grab_byte(v)
			return math.floor(v / 256), string.char(math.floor(v) % 256)
		end
		local sign = 0
		if x < 0 then sign = 1; x = -x end
		local mantissa, exponent = math.frexp(x)
		if x == 0 then -- zero
			mantissa, exponent = 0, 0
		elseif x == 1/0 then
			mantissa, exponent = 0, 2047
		else
			mantissa = (mantissa * 2 - 1) * math.ldexp(0.5, 53)
			exponent = exponent + 1022
		end
		local v, byte = "" -- convert to bytes
		x = mantissa
		for i = 1,6 do
			x, byte = grab_byte(x)
			v = v..byte -- 47:0
		end
		x, byte = grab_byte(exponent * 16 + x)
		v = v..byte -- 55:48
		x, byte = grab_byte(sign * 128 + x)
		v = v..byte -- 63:56
		return v
	end
}

return function(tab)
	local chunk = "";

	local function recurse(tab)
		chunk = chunk .. DumpBinary.SpecString(assert(tab.Name, "Invalid Prototype; proto.Name (nil)"));
		chunk = chunk .. DumpBinary.Int(assert(tab.FirstLine, "Invalid Prototype; proto.FirstLine (nil)"));
		chunk = chunk .. DumpBinary.Int(assert(tab.LastLine, "Invalid Prototype; proto.LastLine (nil"));
		chunk = chunk .. DumpBinary.Int8(assert(tab.NumberOfUpvalues, "Invalid Prototype; proto.NumberOfUpvalues (nil)"))
		chunk = chunk .. DumpBinary.Int8(assert(tab.Arguments, "Invalid Prototype; proto.Arguments (nil)"))
		chunk = chunk .. DumpBinary.Int8(assert(tab.VargFlag, "Invalid Prototype; proto.VargFlag (nil)"))
		chunk = chunk .. DumpBinary.Int8(assert(tab.MaxStackSize, "Invalid Prototype; proto.MaxStackSize (nil)"))

		chunk = chunk .. DumpBinary.Int(assert(tab.Instructions.Count, "Invalid Prototype; proto.NumberOfInstructions (nil)"))
		for i=1, tab.Instructions.Count do
			local ins = tab.Instructions[i - 1];
			chunk = chunk .. OpcodeEncode(ins);
		end

		chunk = chunk .. DumpBinary.Int(assert(tab.Constants.Count, "Invalid Prototype; proto.NumberOfConstants (nil)"))
		for i=1, tab.Constants.Count do
			local k = tab.Constants[i-1];

			if k.ConstantType == "nil" then
				chunk = chunk .. DumpBinary.Int8(0);
			elseif k.ConstantType == "bool" then
				chunk = chunk .. DumpBinary.Int8(1)
				chunk = chunk .. DumpBinary.Int8(k.Value and 1 or 0)
			elseif k.ConstantType == "number" then
				chunk = chunk .. DumpBinary.Int8(3)
				chunk = chunk .. DumpBinary.Float64(k.Value)
			elseif k.ConstantType == "string" then
				chunk = chunk .. DumpBinary.Int8(4)
				chunk = chunk .. DumpBinary.String(k.Value)
			end
		end

		chunk = chunk .. DumpBinary.Int(assert(tab.Protos.Count, "Invalid Prototype; proto.NumberOfProtos (nil)"))
		for i = 1, tab.Protos.Count do
			recurse(tab.Protos[i-1])
		end

		local size = tab.Instructions[0].LineNumber and tab.Instructions.Count or 0
		chunk = chunk .. DumpBinary.Int(size)
		for i = 1, size do
			local instr = tab.Instructions[i - 1]
			chunk = chunk .. DumpBinary.Int(instr.LineNumber)
		end

		chunk = chunk .. DumpBinary.Int(assert(tab.Locals.Count, "Invalid Prototype; proto.NumberOfLocals (nil)"));
		for i = 1, tab.Locals.Count do
			local l = tab.Locals[i-1];
			chunk = chunk .. DumpBinary.String(assert(l.Name, "Invalid Local; local.Name (nil)"))
			chunk = chunk .. DumpBinary.Int(assert(l.SPC, "Invalid Local; local.SPC (nil)"))
			chunk = chunk .. DumpBinary.Int(assert(l.EPC, "Invalid Local; local.EPC (nil)"))
		end

		chunk = chunk .. DumpBinary.Int(assert(tab.Upvalues.Count, "Invalid Prototype; proto.NumberOfUpvalues (nil)"))
		for i = 1, tab.Upvalues.Count do
			chunk = chunk .. DumpBinary.String(assert(tab.Upvalues[i-1].Name, "Invalid Upvalue; upval.Name (nil)"));
		end
	end

	local header = string.dump(function() end):sub(1, 12)
	chunk = chunk .. header

	-- Integer
	if header:sub(8,8) == "\4" then
		DumpBinary.Int = DumpBinary.Int32
	elseif header:sub(8,8) == "\8" then
		DumpBinary.Int = DumpBinary.Int64
	else
		error("Incompatible platform")
	end

	-- Size_t
	if header:sub(9,9) == "\4" then
		DumpBinary.SizeT = DumpBinary.Int32
	elseif header:sub(9,9) == "\8" then
		DumpBinary.SizeT = DumpBinary.Int64
	else
		error("Incompatible platform")
	end

	-- No platform I know of bothers with non-dword sized instructions
	-- Besides, the encoding is in a different table

	-- Not going to bother with number encoding yet.

	recurse(tab.Main)

	return chunk;
end