local Delink  = require "current.delink"
local OpSpecs = require "current.opcodes"

-- Parameter decoding
local dis_param_btable = {}
dis_param_btable[0] = function(proto, p) return "" end
dis_param_btable[1] = function(proto, p) return p end
dis_param_btable[2] = function(proto, p)
	local k = proto.Constants[p]
	if k.ConstantType == "nil" then
		return "<nil>"
	elseif k.ConstantType == "bool" then
		return ("<%s>"):format(tostring(k.Value))
	elseif k.ConstantType == "number" then
		return ("<%s>"):format(k.Value)
	elseif k.ConstantType == "string" then
		return ("<%q>"):format(k.Value)
	end
end
dis_param_btable[3] = function(proto, p)
	if p > 255 then
		return dis_param_btable[2](proto, p-256)
	else
		return p
	end
end
dis_param_btable[4] = dis_param_btable[1]
dis_param_btable[5] = dis_param_btable[1]
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
	local format

	a = dis_param(proto, instr, 1)
	if spec[4] == 2 then
		format = "%s %s, %s"
		b = dis_param(proto, instr, 2)
	elseif spec[4] == 3 then
		format = "%s %s, %s"
		b = dis_param(proto, instr, 2)
		c = dis_param(proto, instr, 3)
	end

	return format:format(opcode, a, b, c)
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

local function disassemble(input)
	if type(input) == "function" then
		input = string.dump(input)
	elseif type(input) ~= "string" or input:sub(1,1) ~= "\27" then
		error("Invalid input", 2)
	end
	local chunk = Delink(input)
	local output = dis_proto(chunk.Main)
	
	return output
end

return {
	disassemble = disassemble,
	dis_proto   = dis_proto,
	dis_instr   = dis_instr,
	dis_param   = dis_param,
}