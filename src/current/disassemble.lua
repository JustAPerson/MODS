local Delink  = require "delink"
local OpSpecs = require "opcodes"

-- Parameter decoding
local dis_param_btable
dis_param_btable = {
	[0] = function(proto, p) return "" end,
	[1] = function(proto, p) return p end,
	[2] = function(proto, p)
		local k = proto.Constants[p]
		if k.ConstantType == "nil" then
			return "<nil>"
		elseif k.ConstantType == "bool" or k.ConstantType == "number" then
			return ("<%s>"):format(k.Value)
		elseif k.ConstantType == "string" then
			return ("<%q>"):format(k.Value)
		end
	end,
	[3] = function(proto, p)
		if p > 255 then
			return dis_param_btable[2](proto, p-256)
		else
			return p
		end
	end,
	[4] = dis_param_btable[1],
	[5] = dis_param_btable[1],
}
local function dis_param(proto, instr, n)
	local spec = OpSpecs[instr.Opcode]
	if n > spec[4] then return "" end
	 
	local target = spec[3][n]
	local p = instr[OpSpecs.list[n]]	-- A, B, or C

	return dis_param_btable[target](proto, p)
end

-- Instruction decoding 
local function dis_instr(proto, instr)
	local opcode = instr.Opcode
	local spec = OpSpecs[opcode]
	local a, b, c = "", "", ""

	a = dis_param(proto, instr, 1)
	if spec[4] == 2 then
		b = dis_param(proto, instr, 2)
	elseif spec[4] == 3 then
		b = dis_param(proto, instr, 2)
		c = dis_param(proto, instr, 3)
	end

	return opcode, a, b, c
end

-- Function decoding
local function dis_proto(proto, level)
	level = level or 0
	local output = ""

	local function emit(str, ...)
		output = ("%s\n%s%s"):format(output, ("\t"):rep(level), str:format(...))	
	end

	if proto.Name ~= "" then
		emit(".name %q", proto.Name)
	end

	emit(".options %d, %d, %d, %d", proto.NumberOfUpvalues,	proto.Arguments,
		proto.VargFlag, proto.MaxStackSize)

	local statements = {}
	for ip, instr in next, proto.Instructions do
		statements[ip] = dis_instr(proto, instr)
	end
	
	-- FIXME labels
	for i, v in next, statements do
		emit(v)
	end
end

return function(bytecode)
	local chunk = Delink(bytecode)
	local output = dis_proto(chunk.Main)
	
	return output
end