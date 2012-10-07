local bit = require "current.bit"
local OpSpecs = require "current.opcodes"
return function(chunk)
	local bit = bit;
	local index = 1
	local tab = {}
	local big = false;

	local function GetInt8()
		local a = chunk:sub(index, index):byte()
		index = index + 1
		return a
	end
	local function GetInt16(str, inx)
		local a = GetInt8()
		local b = GetInt8()
		return 256*b + a
	end
	local function GetInt32(str, inx)
		local a = GetInt16()
		local b = GetInt16()
		return 65536*b + a
	end
	local function GetInt64()
		local a = GetInt32()
		local b = GetInt32()
		return 2^32*b + a
	end
	local function GetFloat64()
		local a = GetInt32()
		local b = GetInt32()
		if a == b and a == 0 then
			return 0;
		else
			return (-2*bit.get(b, 32)+1)*(2^(bit.get(b, 21, 31)-1023))*((bit.get(b, 1, 20)*(2^32) + a)/(2^52)+1)
		end
	end

	local function GetStringRaw(len)
		local str = chunk:sub(index, index+len-1)
		index = index + len
		return str
	end

	local GetInt, GetSizeT

	local function GetString()
		return GetStringRaw(GetSizeT()):sub(1,-2)
	end

	local function GetTypeFunction()
		local tab = {}
		tab.Name = GetString();
		tab.FirstLine = GetInt();
		tab.LastLine = GetInt();
		GetInt8() -- Upvalues
		tab.Arguments = GetInt8();
		tab.VargFlag = GetInt8()
		tab.MaxStackSize = GetInt8()


		do
			local instructions = {};
			local num = GetInt()

			tab.NumberOfInstructions = num;
			instructions.Count = num;

			for i = 1, num do
				local instr = {};
				local op = GetInt32();
				local opcode = bit.get(op, 1, 6)
				instr.Number = i;
				instr.Opcode =  OpSpecs[opcode]
				instr.OpcodeType = OpSpecs.enum[OpSpecs[instr.Opcode][2]]
				if instr.OpcodeType == "ABC" then
					instr.A = bit.get(op, 7, 14)
					instr.B = bit.get(op, 24, 32)
					instr.C = bit.get(op, 15, 23)
				elseif instr.OpcodeType == "ABx" then
					instr.A = bit.get(op, 7, 14)
					instr.B = bit.get(op, 15, 32)
				elseif instr.OpcodeType == "AsBx" then
					instr.A = bit.get(op, 7, 14)
					instr.B = bit.get(op, 15, 32) - 131071
				end

				instructions[i-1] = instr;
			end

			tab.Instructions = instructions;
		end

		do
			local constants = {};
			local num = GetInt()

			tab.NumberOfConstants = num;
			constants.Count = num;

			for i = 1, num do
				local k = {};
				local ty = GetInt8()

				k.Number = i-1;

				if ty == 0 then
					k.ConstantType = "nil";
					k.Value = ""
				elseif ty == 1 then
					k.ConstantType = "bool"
					k.Value = GetInt8() ~= 0
				elseif ty == 3 then
					k.ConstantType = "number"
					k.Value = GetFloat64();
				elseif ty == 4 then
					k.ConstantType = "string"
					k.Value = GetString();
				end
				constants[i-1] = k;
			end
			tab.Constants = constants;
		end

		do
			local protos = {};
			local num = GetInt()

			tab.NumberOfProtos = num;
			protos.Count = num;

			for i = 1, num do
				protos[i-1] = GetTypeFunction()
			end

			tab.Protos = protos
		end
		do
			local numsrc = GetInt()

			for i = 1, numsrc do
				tab.Instructions[i-1].LineNumber = GetInt();
			end

			local locals = {};
			local numlocal = GetInt()
			tab.NumberOfLocals = numlocal;
			locals.Count = numlocal;
			tab.Locals = locals;

			for i = 1, numlocal do
				locals[i-1] = {
					Name = GetString(),
					SPC = GetInt(),
					EPC = GetInt(),
				};
			end

			local numups = GetInt()
			local ups = {Count = numups}
			tab.NumberOfUpvalues = numups;
			tab.Upvalues = ups;

			for i = 1, numups do
				ups[i-1] = {Name = GetString()};
			end
		end

		return tab;
	end

	--assert(chunk:sub(1,12) == '\27Lua\81\0\1\4\4\4\8\0', "Unsupported Bytecode format")
	tab.Identifier = GetStringRaw(4)
	tab.Version = GetInt8()
	tab.Format = GetInt8() == 0 and "Official" or "Unofficial"
	tab.BigEndian = GetInt8() == 0
	tab.IntSize = GetInt8()
	tab.SizeT = GetInt8()
	tab.InstructionSize = GetInt8()
	tab.NumberSize = GetInt8()
	tab.FloatingPoint = GetInt8() == 0

	-- TODO refactor this
	assert(tab.Identifier == "\27Lua", "Unsupported Bytecode format")
	assert(tab.Version == 0x51, "Unsupported Bytecode format")
	assert(not tab.BigEndian, "Unsupported Bytecode format")
	assert(tab.IntSize == 4 or tab.IntSize == 8, "Unsupported Bytecode format")
	assert(tab.SizeT   == 4 or tab.SizeT   == 8, "Unsupported Bytecode format")
	assert(tab.InstructionSize == 4, "Unsupported Bytecode format")
	assert(tab.NumberSize == 8, "Unsupported Bytecode format")
	assert(tab.FloatingPoint, "Unsupported Bytecode format")

	if tab.IntSize == 4 then
		GetInt = GetInt32
	elseif tab.IntSize == 8 then
		GetInt = GetInt64
	end

	if tab.SizeT == 4 then
		GetSizeT = GetInt32
	elseif tab.SizeT == 8 then
		GetSizeT = GetInt64
	end

	tab.Main = GetTypeFunction()
	return tab;
end