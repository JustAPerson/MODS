local OpSpecs = {
	[0] = "MOVE",
	"LOADK",
	"LOADBOOL",
	"LOADNIL",
	"GETUPVAL",
	"GETGLOBAL",
	"GETTABLE",
	"SETGLOBAL",
	"SETUPVAL",
	"SETTABLE",
	"NEWTABLE",
	"SELF",
	"ADD",
	"SUB",
	"MUL",
	"DIV",
	"MOD",
	"POW",
	"UNM",
	"NOT",
	"LEN",
	"CONCAT",
	"JMP",
	"EQ",
	"LT",
	"LE",
	"TEST",
	"TESTSET",
	"CALL",
	"TAILCALL",
	"RETURN",
	"FORLOOP",
	"FORPREP",
	"TFORLOOP",
	"SETLIST",
	"CLOSE",
	"CLOSURE",
	"VARARG",

	-- opcode = {opcode_number, enum_type,  {parameter_types}, number_of_params}
	enum = {
		[0] = "ABC",
		"ABx",
		"AsBx",
	},
	list = {
		"A",
		"B",
		"C",
	},
	-- Parameter types (likely to change):
		-- Unused/Arbitrary = 0
		-- Register 		= 1
		-- Constant 		= 2
		-- Constant/Register= 3
		-- Upvalue 			= 4
		-- Jump Distace 	= 5

	MOVE			= {0, 0,	{1, 1, 5},	2};
	LOADK			= {1, 1,	{1, 2},		2};
	LOADBOOL		= {2, 0,	{1, 0, 0},	3};
	LOADNIL			= {3, 0,	{1, 1, 1},	3};
	GETUPVAL		= {4, 0,	{1, 4, 5},  2};
	GETGLOBAL		= {5, 1,	{1, 2},		2};
	GETTABLE		= {6, 0,	{1, 1, 3},	3};
	SETGLOBAL		= {7, 1,	{1, 2},		2};
	SETUPVAL		= {8, 0,	{1, 4, 5},	2};
	SETTABLE		= {9, 0,	{1, 3, 3},	3};
	NEWTABLE		= {10, 0,	{1, 0, 0},	3};
	SELF			= {11, 0,	{1, 1, 3},	3};
	ADD				= {12, 0,	{1, 1, 3},	3};
	SUB				= {13, 0,	{1, 1, 3},	3};
	MUL				= {14, 0,	{1, 1, 3},	3};
	DIV				= {15, 0,	{1, 1, 3},	3};
	MOD				= {16, 0,	{1, 1, 3},	3};
	POW				= {17, 0,	{1, 1, 3},	3};
	UNM				= {18, 0,	{1, 1, 5},	2};
	NOT				= {19, 0,	{1, 1, 5},	2};
	LEN				= {20, 0,	{1, 1, 5},	2};
	CONCAT			= {21, 0,	{1, 1, 1},	3};
	JMP				= {22, 2,	{0, 5},		1};
	EQ				= {23, 0,	{1, 3, 3},	3};
	LT				= {24, 0,	{1, 3, 3},	3};
	LE				= {25, 0,	{1, 3, 3}, 	3};
	TEST			= {26, 0,	{1, 5, 1},	2};
	TESTSET			= {27, 0,	{1, 1, 1},	3};
	CALL			= {28, 0,	{1, 0, 0},	3};
	TAILCALL		= {29, 0,	{1, 0, 0},	3};
	RETURN			= {30, 0,	{1, 0, 5},	2};
	FORLOOP			= {31, 2,	{1, 0},		2};
	FORPREP			= {32, 2,	{1, 0},		2};
	TFORLOOP		= {33, 0,	{1, 5, 0},	2};
	SETLIST			= {34, 0,	{1, 0, 0},	3};
	CLOSE			= {35, 0,	{1, 5, 5},	1};
	CLOSURE			= {36, 1,	{1, 0},		2};
	VARARG			= {37, 0,	{1, 1, 5},	2};
}

local bit;
bit = {
	new = function(str)
		return tonumber(str, 2)
	end,
	get = function(num, n, n2)
		if n2 then
			local total = 0
			local digitn = 0
			for i = n, n2 do
				total = total + 2^digitn*bit.get(num, i)
				digitn = digitn + 1
			end
			return total
		else
			local pn = 2^(n-1)
			return (num % (pn + pn) >= pn) and 1 or 0
		end
	end,
	getstring = function(num, mindigit, sep)
		mindigit = mindigit or 0
		local pow = 0
		local tot = 1
		while tot <= num do
			tot = tot * 2
			pow = pow + 1
		end
		---
		if pow < mindigit then pow = mindigit end
		---
		local str = ""
		for i = pow, 1, -1 do
			str = str..bit.get(num, i)..(i==1 and "" or (sep or "-"))
		end
		return str
	end
}


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
		return #s == 0 and "\0\0\0\0" or DumpBinary.SizeT(#s+1) .. s .. "\0";
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

local function Link(tab)
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

		chunk = chunk .. DumpBinary.Int(tab.Instructions.Count)
		for i = 1, tab.Instructions.Count do
			chunk = chunk .. DumpBinary.Int(assert(tab.Instructions[i - 1].LineNumber, "Invalid Instruction; instr.LineNumber (nil)"))
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

local function Delink(chunk)
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
	local function GetFloat64()
		local a = GetInt32()
		local b = GetInt32()
		if a == b and a == 0 then
			return 0;
		else
			return (-2*bit.get(b, 32)+1)*(2^(bit.get(b, 21, 31)-1023))*((bit.get(b, 1, 20)*(2^32) + a)/(2^52)+1)
		end
	end

	local function GetString(len)
		local str = chunk:sub(index, index+len-1)
		index = index + len
		return str
	end

	local function GetTypeInt()
		local a = GetInt8()
		local b = GetInt8()
		local c = GetInt8()
		local d = GetInt8()
		return d*16777216 + c*65536 + b*256 + a
	end
	local function GetTypeString()
		local tmp = GetInt32()
		if tab.SizeT == 8 then GetInt32() end
		return GetString(tmp)
	end
	local function GetTypeFunction()
		local tab = {}
		tab.Name = GetTypeString():sub(1, -2);
		tab.FirstLine = GetTypeInt();
		tab.LastLine = GetTypeInt();
		GetInt8() -- Upvalues
		tab.Arguments = GetInt8();
		tab.VargFlag = GetInt8()
		tab.MaxStackSize = GetInt8()


		do
			local instructions = {};
			local num = GetInt32()

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
			local num = GetInt32()

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
					k.Value = GetTypeString():sub(1,-2);
				end
				constants[i-1] = k;
			end
			tab.Constants = constants;
		end

		do
			local protos = {};
			local num = GetInt32()

			tab.NumberOfProtos = num;
			protos.Count = num;

			for i = 1, num do
				protos[i-1] = GetTypeFunction()
			end

			tab.Protos = protos
		end
		do
			local numsrc = GetInt32()

			for i = 1, numsrc do
				tab.Instructions[i-1].LineNumber = GetInt32();
			end

			local locals = {};
			local numlocal = GetInt32()
			tab.NumberOfLocals = numlocal;
			locals.Count = numlocal;
			tab.Locals = locals;

			for i = 1, numlocal do
				locals[i-1] = {
					Name = GetTypeString():sub(1,-2),
					SPC = GetInt32(),
					EPC = GetInt32(),
				};
			end

			local numups = GetInt32()
			local ups = {Count = numups}
			tab.NumberOfUpvalues = numups;
			tab.Upvalues = ups;

			for i = 1, numups do
				ups[i-1] = {Name = GetTypeString():sub(1, -2)};
			end
		end

		return tab;
	end

	assert(chunk:sub(1,12) == '\27Lua\81\0\1\4\4\4\8\0', "Unsupported Bytecode format")

	tab.Identifier = GetString(4)
	tab.Version = GetInt8()
	tab.Format = GetInt8() == 0 and "Official" or "Unofficial"
	tab.BigEndian = GetInt8() == 0
	tab.IntSize = GetInt8()
	tab.SizeT = GetInt8()
	tab.InstructionSize = GetInt8()
	tab.NumberSize = GetInt8()
	tab.FloatingPoint = GetInt8() == 0
	tab.Main = GetTypeFunction()
	return tab;
end

local function exception(o)
	error( "["..o.type .. "]:" .. o.pos.line ..":" ..o.pos.char ..": " .. o.msg, 0)
end

local function	warn(o)
	print( "[" .. o.type .. "] :" .. (o.pos or "unknown") ..": " .. o.msg)
end

local function try(f, e)
	local res = {pcall(f)}
	if not res[1] then
		if type(err) == "table" then
			e(res[2])
		else
			error(err)
		end
	end
end

local function lookupify(tb)
	for k, v in pairs(tb) do
		tb[v] = true
	end
	return tb
end

local function trueFunc()
	return true
end

local function falseFunc()
	return false
end

local isWhiteLookup = {["\n"] = true, ["\t"] = true, [" "] = true}
local function isWhite(ch)
	return isWhiteLookup[ch]
end

local isDigitLow = ("0"):byte()
local isDigitHigh = ("9"):byte()
local function isDigit(ch)
	local n = ch:byte()
	return n >= isDigitLow and n <= isDigitHigh
end

local isAlphaLow = ("a"):byte()
local isAlphaHigh = ("z"):byte()
local function isAlpha(ch)
	local n = ch:lower():byte()
	return (n >= isAlphaLow and n <= isAlphaHigh) or n == 95
end

local directives = {}

--
-- TokenStream class
--
local function MakeStream(str)
	local len = #str
	local ptr = 1
	local line = 1
	local char = 0

	--=======================================
	--utility for operating on input string
	--=======================================

	--is at eof?
	local function eof()
		return ptr >= len
	end

	--look at current char, leave stream at is it
	local function peek(n)
		n = n or 0
		return eof() and "\0" or str:sub(ptr+n,ptr+n)
	end

	--look at current char and move to next in stream
	local function get(ch)
		if ch then
			if peek() == ch then
				get()
				return true
			else
				return false
			end
		else
			if eof() then return "\0" end
			local c = peek()
			ptr = ptr + 1
			if c == "\n" then
				line = line + 1
				char = 0
			else
				char = char + 1
			end
			return c
		end
	end

	--get a mark for the current position
	local function mark()
		return {line=line,char=char,ptr=ptr}
	end

	--restore the position to a given mark
	local function reset(pos)
		line = pos.line
		char = pos.char
		ptr = pos.ptr
	end

	--check if a character is an xxx character
	--check the current character if none is given
	local function white(ch)
		return isWhite(ch or peek())
	end

	local function alpha(ch)
		return isAlpha(ch or peek())
	end

	local function digit(ch)
		return isDigit(ch or peek())
	end

	local function is(ch)
		return peek() == ch
	end

	--strip all of the next whitespace characters
	local function strip()
		while white() do
			get()
		end
	end

	--========================================
	-- Meat and bones, generate the tokens
	--========================================

	--token table
	local tokens = {}
	setmetatable(tokens, {__index=function() return tokens[#tokens] end})

	local keywords = {
		".local",
		".proto",
		".end",
		".options",
		".upval",
		".macro",
	};
	lookupify(keywords);

	--symbol lookups
	local symbols1 = {
		":", ";", ".", ",",
		"(", ")", "[", "]", "{", "}",
		"=", "+", "-", "/", "*", "%", "^", "<", ">"
	}
	lookupify(symbols1)
	local symbols2 = {
		"==", "~=", ">=", "<=",
	}
	lookupify(symbols2)
	--push a token between a begining and end pos in the input
	local function pushToken(tk, tst, tend)
		tk.tstart = tst
		tk.tend = tend
		--
		tk.isSymbol = (tk.type == "symbol") and function(sy)
			if sy then
				return tk.data == sy;
			else
				return true;
			end
		end or falseFunc
		--
		tk.isKeyword = (tk.type == "keyword") and function(kw)
			if kw then
				return tk.data == kw
			else
				return true;
			end
		end or falseFunc
		--
		tk.isIdent = (tk.type == "ident") and trueFunc or falseFunc
		tk.isString = (tk.type == "string") and trueFunc or falseFunc
		tk.isNumber = (tk.type == "number") and trueFunc or falseFunc
		tk.isEof = (tk.type == "eof") and trueFunc or falseFunc
		--
		tokens[#tokens+1] = tk
	end

	while not eof() do
		strip()
		--all tokens need the begining, mark it here
		local tbegin = mark()
		if alpha() then
			local ident = ""
			while alpha() or digit() do
				ident = ident..get()
			end

			pushToken({type="ident", data=ident}, tbegin, mark())
		elseif digit() or (is("-") and isDigit(peek(1))) then
			local num = ""
			if is("0") and peek(1) == "x" then
				--number in the format: `0x` <digits>
				num = "0x"
				if not digit() then
					exception{type="lex", msg="Expected digits after `0x`", pos=mark()}
				end
				while digit() do
					num = num..get()
				end
			else
				--number in the format: [ `-` ] <digits> [ `.` <digits> ] [ `e` [ `-` ] <digits> ]
				if get("-") then
					num = num.."-"
				end
				while digit() do
					num = num..get()
				end
				if get(".") then
					num = num.."."
				end
				if get(".") then
					exception{type="lex", msg="Expected digits after `.`", pos=mark()}
				end
				while digit() do
					num = num..get()
				end
				if get("e") then
					num = num.."e"
					if get("-") then
						num = num.."-"
					end
					while digit() do
						num = num..get()
					end
				end
			end
			pushToken({type="number", data=num}, tbegin, mark())
		elseif peek() == "\"" or peek() == "\'" then
			local lookFor = get()
			--string, containing escape characters
			local str = ""
			while not get(lookFor) do
				--unfinished string
				if eof() then
					exception{type="lex", msg="Unfinished string (start: " .. tbegin.line ..":" .. tbegin.char .. ") near <eof>", pos=mark()}
				end
				--handle escape chars
				if get("\\") then
					--unfinished escape sequence
					if eof() then
						exception{type="lex", msg="Unfinished escape sequence", pos=mark()}
					end
					local c = get()
					if c == "n" then
						str = str.."\n"
					elseif c == "r" then
						str = str.."\r"
					elseif c == "t" then
						str = str.."\t"
					elseif c == "\\" then
						str = str.."\\"
					elseif c == "0" then
						str = str.."\0"
					else
						exception{type="lex", msg="Bad escape sequence `\\"..c.."`", pos=mark()}
					end
				else
					--normal string character
					str = str..get()
				end
			end
			pushToken({type="string", data=str, lookFor = lookFor}, tbegin, mark())
		else
			--one or two character symbol
			local sy1 = peek()
			local sy2 = peek()..peek(1)

			if sy1 == "." and alpha(peek(1)) then	-- directives, gonna call 'em keywords though
				local ident = sy2;

				get();
				get();

				while alpha() do
					ident = ident .. get();
				end

				pushToken({type="keyword", data = ident}, tbegin, mark());
			elseif symbols2[sy2] then
				get()
				get()
				pushToken({type="symbol", data=sy2}, tbegin, mark())
			elseif symbols1[sy1] then
				get();
				pushToken({type="symbol", data=sy1}, tbegin, mark())
			else
				if not eof() then
					get();
					exception{type="lex", msg="Bad input character `" .. sy1:byte() .. "`", pos=mark()}
				end
			end
		end
	end
	pushToken({type="eof"}, mark(), mark())

	--============================================
	-- access, provide token stream functionality
	--============================================

	local this = {tokens = tokens}
	local mTPos = 1
	local mEofToken = tokens[#tokens]

	this.getPos = function()
		return mTPos;
	end

	this.reset = function(pos)
		mTPos = pos or 0
	end

	this.eof = function()
		return mTPos >= #tokens
	end

	this.peek = function(n)
		n = n or 0
		return tokens[mTPos+n]
	end

	this.mark = function()
		return this.peek().tend;
	end

	this.get = function()
		local tk = tokens[mTPos]
		mTPos = mTPos + 1
		return tk
	end

	this.isSymbol = function(...)
		return this.peek().isSymbol(...)
	end

	this.isKeyword = function(...)
		return this.peek().isKeyword(...)
	end

	this.isNumber = function()
		return this.peek().isNumber()
	end

	this.isString = function()
		return this.peek().isString()
	end

	this.isIdent = function()
		return this.peek().isIdent()
	end

	this.isEof = function()
		return this.peek().isEof()
	end

	this.consumeSymbol = function(sy)
		if this.isSymbol(sy) then
			this.get()
			return true
		else
			return false
		end
	end

	this.consumeIdent = function()
		if this.isIdent() then
			this.get();
			return true;
		else
			return false;
		end
	end

	this.consumeString = function()
		if this.isString() then
			this.get();
			return true;
		else
			return false;
		end
	end

	this.consumeKeyword = function(kw)
		if this.isKeyword(kw) then
			this.get();
			return true
		else
			return false;
		end
	end

	this.expectIdent = function(err)
		if this.isIdent() then
			return this.get()
		else
			err = err or "Expected <ident>"
			exception{type="assembler", msg=err, pos=this.mark()}
		end
	end

	this.expectSymbol = function(sy, err)
		if this.isSymbol(sy) then
			return this.get()
		else
			err = err or "Expected " .. (sy and "`" .. sy .. "`") or "<symbol>";
			exception{type="assembler", msg=err, pos=this.mark()}
		end
	end

	this.expectNumber = function(err)
		if this.isNumber() then
			return this.get()
		else
			err = err or "Expected <number>"
			exception{type="assembler", msg=err, pos=this.mark()}
		end
	end

	this.expectKeyword = function(kw, err)
		if this.isKeyword(kw) then
			return this.get()
		else
			err = err or "Expected <keyword>"
			exception{type="assembler", msg=err, pos=this.mark()}
		end
	end

	this.expectString = function(s, err)
		if this.isString(s) then
			return this.get()
		else
			err = err or "Expected <string>"
			exception{type="assembler", msg=err, pos=this.mark()}
		end
	end

	return this
end

local directives = {
	[".local"] = function(stream, state)
		local name = stream.expectString().data;
		local sy, n1, n2 = stream.peek(), stream.peek(1), stream.peek(3)
		local spc, epc;

		if n1.isNumber() and n2.isNumber() then
			for i = 1, 4 do
				stream.get()
			end

			if sy.isSymbol("(") then
				stream.expectSymbol(")", "`)` expected to close `(` in after `.local` directive");
			end

			spc, epc = n1, n2;
		else
			spc, epc = 0, 0;
		end

		state.Current:PushIdent{
			type = "local";
			name = name;
			val = state.Current:PushLocal{
				Name = name;
				SPC = spc;
				EPC = epc;
			}
		};
	end,
	[".proto"] = function(stream, state)
		state.Current:NewProto();
	end,
	[".end"] = function(stream, state)
		state.Current.LastLine = stream.mark().line;
		state.Current = state.Current.Parent;
	end,
	[".upval"] = function(stream, state)
		local name = stream.expectString("`string` expected after directive `.upval`").data;

		state.Current:PushUpval{
			Name = name;
		}
	end,
	[".options"] = function(stream, state)
		local msg = "directive `.options` requires 4 parameters seperated by commas";

		state.Current.NumberOfUpalues = stream.expectNumber(msg).data;
		stream.expectSymbol(",", msg);
		state.Current.Arguments = stream.expectNumber(msg).data;
		stream.expectSymbol(",", msg);
		state.Current.VargFlag = stream.expectNumber(msg).data;
		stream.expectSymbol(",", msg);
		state.Current.MaxStackSize = stream.expectNumber(msg).data;

		state.Current.Options = true;
	end,
	[".name"] = function(stream, state)
		local name = stream.expectString();

		state.Current.Name = name;
		state.Current:PushIdent{
			type = "proto";
			name = name;
			value = state.Current.num;
		};
	end,
	[".macro"] = function(stream, state)
		-- TODO .macro
		-- Remember, no allowing of macros starting with "_"

		local name = stream.expectIdent().data;
		local params = {};

		if name:sub(1,1) == "_" then
			exception{
				type="assembler",
				pos = stream.mark(),
				msg = "Macro identifiers cannot start with an underscore",
			}
		end

		local l = stream.expectSymbol("(", "`(` expected after " .. name);

		while true do
			if stream.isIdent() then
				params[#params + 1] = stream.get().data;
			elseif stream.consumeSymbol(")") then
				break;
			elseif stream.eof() then
				exception{
					type = "assembler";
					pos = stream.mark();
					msg = "`)` expected to close `(` on line " .. l.tend.line;
				}
			elseif not stream.consumeSymbol(",") then
				exception{
					type="assembler";
					pos = stream.mark();
					msg = "Unexpected symbol `" .. stream.peek().data .. "`";
				}
			end
		end

		local macro = {params = params, count = #params};
		local depth = 1;

		stream.expectSymbol("{");
		macro.start = stream.getPos();

		while true do
			local token = stream.get();

			if token.isSymbol("{") then
				depth = depth + 1;
			elseif token.isSymbol("}") then
				depth = depth - 1;
			end

			if depth == 0 then
				break;
			else
				macro.stop = stream.getPos();
			end
		end

		if not state.Macros[name] then
			local t = {};
			t[#params] = macro;
			state.Macros[name] = t;
		else
			if not state.Macros[name][#params] then
				state.Macros[name][#params] = macro;
			else
				warn{
					type="assembler",
					pos = stream.mark().line,
					msg = "Overwriting pre-existing macro `" .. name .. "`",
				}
			end
		end
	end,
	[".const"] = function(stream, state)
		local value = stream.get();

		if value.isString() or value.isNumber or value.isIdent() then
			if value.isIdent() and (value.data == "true" or value.data == "false" or value.data == "nil") then
				return state.Current:PushIdent{
					name = "k" .. (state.Current.Constants[0] and #state.Current.Constants or 0);
					type = "const";
					value = value.data
				};
			end
		end
		exception{
			type = "assembler",
			pos = value.tend;
			msg = "Invalid constant value `" .. value.data .. "`";
		}
	end,
};

local function ParseStream(stream)
	local state;

	local function PushInstr(proto, int, a, b, c)
		local x = proto.Instructions;
		local y;
		local pos = stream.mark();
		proto.pc = proto.pc + 1;

		x.Count = x.Count + 1;

		y = {
			pc = proto.pc;
			Opcode = int:upper();
			OpcodeType = OpSpecs.enum[OpSpecs[int:upper()][2]];
			A = a;
			B = b;
			C = c;
			LineNumber = pos.line;

			Resolve = function(this)
				local x;
				local env = setmetatable({}, {__index = function(a, b)
					if proto.Idents[b] then
						return proto.Idents[b]:Resolve(y, pos, x);
					else
						exception{type="assembler", pos=pos, msg="Unknown identifier `" .. b .. "`" }
					end
				end});

				local test = function(str, _)
					x = _;
					local Func = function(_, a)
						local choice = OpSpecs[this.Opcode][3][x];

						if choice ~= 2 and choice ~= 3 then
							exception{
								type="assembler";
								pos = pos;
								msg = "The #"..x.." parameter of `" .. this.Opcode .. "` does not take a constant";
							};
						end

						local i;
						for z = 1, #proto.Constants do
							local v = proto.Constants[z - 1];

							if v == a then
								return (choice == 2) and z - 1 or (choice == 3) and z + 255;
							end
						end

						local f, e = loadstring("return " .. a);

						if f then
							f, e = pcall(f);

							if not f then
								error("I have no clue when this might happen, so contact me if it does");
							end
						else
							error("I have no clue when this might happen, so contact me if it does");
						end

						local i = proto:PushConst{
							ConstantType = type(e);
							Value = e;
						}

						return (choice == 2) and i or (choice == 3) and i + 256;
					end
					str = str:gsub("(<(.-)>)", Func);
					local f, e = loadstring("return " .. str);

					if f then
						setfenv(f, env);
						f, e = pcall(f)

						if f then
							return e;
						else
							exception{type="assembler", pos=pos, msg=e};
						end
					else
						exception{type="assembler", pos=pos, msg=e};
					end
				end

				for i, v in ipairs(OpSpecs[this.Opcode][3]) do
					local letter = OpSpecs.list[i];

					if v ~= 5 then
						this[letter] = test(this[letter], i);
					else
						this[letter] = 0;
					end
				end
			end
		};

		x[x[0] and #x+1 or 0] = y;
	end

	local function PushIdent(this, val)
		local a = this.Idents;

		if a[val.name] then
			warn{
				type = "assembler",
				pos=stream.mark().line,
				msg = "Overwriting pre-existing Identifer `" .. val.name .. "`"
			}
		end

		val.pc = this.pc;
		val.start = stream.mark();

		function val.Resolve(this, instr, start, x)
			if this.type == "label" then
				return instr.pc + 1 - this.pc;
			elseif this.type == "const"  then
				local choice = OpSpecs[intstr.Opcode][3][x];

				if choice ~= 2 and choice ~= 3 then
					exception{
						type="assembler";
						pos = pos;
						msg = "The #"..x.." parameter of `" .. this.Opcode .. "` does not take a constant";
					};
				end

				return choice == 2 and this.value - 1 or this.value + 255;
			else
				return this.value;
			end
		end

		a[val.name] = val;
	end

	local function PushUpval(this, val)
		local a = this.Upvalues;

		a.Count = a.Count + 1;
		a[a[0] and #a+1 or 0] = val

		return #a;
	end

	local function PushLocal(this, val)
		local a = this.Locals;

		a.Count = a.Count + 1;
		a[a[0] and #a+1 or 0] = val;

		return #a;
	end

	local function PushConst(this, val)
		local a = this.Constants;

		a.Count = a.Count + 1;
		a[a[0] and #a+1 or 0] = val;

		return #a;
	end

	local function NewProto(this)
		local a = this.Protos;
		local b = {
			Parent = this;
			Idents = {};

			pc = 0;
			num = a[0] and #a+1 or 0;

			NumberOfUpvalues = 0;
			Arguments = 0;
			VargFlag = 0;
			MaxStackSize = 0;

			Name = "";
			FirstLine = stream.mark().line;
			Instructions = {Count = 0};
			Constants = {Count = 0};
			Protos = {Count = 0};
			Locals = {Count = 0};
			Upvalues = {Count = 0};

			NewProto = NewProto;
			PushInstr = PushInstr;
			PushIdent = PushIdent;
			PushUpval = PushUpval;
			PushLocal = PushLocal;
			PushConst = PushConst;
		};

		a.Count = a.Count + 1;

		if a[0] then
			a[#a + 1] = b;
		else
			a[0] = b;
		end

		state.Current = b;

		return #a;
	end

	state = {
		Main = {
			Idents = {};

			pc = 0;

			NumberOfUpvalues = 0;
			Arguments = 0;
			VargFlag = 0;
			MaxStackSize = 0;

			Name = "";
			FirstLine = 0;
			Instructions = {Count = 0};
			Constants = {Count = 0};
			Protos = {Count = 0};
			Locals = {Count = 0};
			Upvalues = {Count = 0};
		},

		Macros = {};
		MacroStack = {};
		InMacro = false;
	}

	state.Main.NewProto = NewProto;
	state.Main.PushInstr = PushInstr;
	state.Main.PushIdent = PushIdent;
	state.Main.PushUpval = PushUpval;
	state.Main.PushLocal = PushLocal;
	state.Main.PushConst = PushConst;

	state.Current = state.Main;

	-- Time to process the tokens
	while not stream.eof() do
		local token = stream.peek();

		if stream.consumeKeyword() then
			--	Token is a Directive
			local dir = token.data;

			if directives[dir] then
				directives[dir](stream, state)
			else
				exception{type = "assembler", pos = stream.mark(), msg = "Attempt to call a nil directive (`" .. dir .."`)"}
			end
		elseif stream.consumeIdent() then
			-- Token is an Identifier
			if stream.consumeSymbol(":") then
				-- Token is a Label
				state.Current:PushIdent{
					name = token.data;
					type = "label";
				};
			else
				local instr = token.data;
				local params = {};
				local expression = {};
				local c, macro;
				local leaveMacro = false;

				while true do
					token = stream.peek();

					if #state.MacroStack > 0 and state.MacroStack[#state.MacroStack][2] == stream.getPos() then
						params[#params + 1] = #expression == 0 and nil or expression;
						leaveMacro = true;
						break;
					end

					if token.isEof() or token.isIdent() or token.isKeyword() then
						if stream.isKeyword() or token.isEof() then
							params[#params + 1] = #expression == 0 and nil or expression;
							expression = {};
							
							break;
						elseif token.isIdent() then
							if state.Macros[token.data] or OpSpecs[token.data:upper()] or stream.peek(1).isSymbol(":") then
								params[#params + 1] = #expression == 0 and nil or expression;
								expression = {};
								
								break;
							else
								stream.get();
								table.insert(expression, token);
							end
						end
					elseif stream.consumeSymbol(",") then
						params[#params + 1] = #expression == 0 and nil or expression;
						expression = {};
					else
						stream.get();
						table.insert(expression, token);
					end
				end

				if instr:sub(1,1) ~= "_" and state.Macros[instr] then
					local v = state.Macros[instr][#params];

					if not v then
						exception{
							type = "assembler",
							pos = stream.mark(),
							msg = "Undeclared Macro `" .. instr .. "` with " .. #params .. " params";
						}
					end

					local newparams = {};

					for i,v in pairs(params) do
						newparams[state.Macros[instr][#params].params[i]] = v;
					end

					state.InMacro = true;
					table.insert(state.MacroStack, {stream.getPos(), v.stop, params = newparams});
					stream.reset(v.start);
				else
					for i,v in pairs(params) do
						local str = "";

						for _, t in pairs(v) do
							if t.isString() then
								str = str .. t.lookFor .. t.data .. t.lookFor;
							elseif state.InMacro and t.isIdent() then
								for _, t in pairs(state.MacroStack[#state.MacroStack].params[t.data]) do
									if t.isString() then
										str = str .. t.lookFor .. t.data .. t.lookFor;
									else
										str = str .. t.data;
									end

									str = str .. " ";
								end
							else
								str = str .. t.data;
							end

							str = str .. " ";
						end

						params[i] = str;
					end

					if #params == OpSpecs[instr:upper()][4] then
						state.Current:PushInstr(instr, unpack(params));
					else
						exception{type="assembler", pos = stream.mark(),
							msg="Invalid number of arguments given to `" .. instr .. "`, got `" .. #params .. "`, expected `" .. OpSpecs[instr:upper()][4] .. "`"};
					end
					
					if leaveMacro then
						stream.reset(table.remove(state.MacroStack)[1]);

						if #state.MacroStack == 0 then
							state.InMacro = false;
						end
					end
				end
			end
		else
			exception{
				type = "assembler",
				pos = stream.mark(),
				msg = "Unexpected <" .. token.type .."> " .. token.data
			}
		end
	end

	local function recurse(proto)
		if not proto.Options then
			warn{
				type="assembler",
				pos = proto.FirstLine;
				msg = "`prototype` has unitiliazed parameters";
			}
		end

		for i = 1, proto.Instructions.Count do
			proto.Instructions[i - 1]:Resolve();
		end

		for i = 1, proto.Protos.Count do
			recurse(proto.Protos[i - 1])
		end;
	end

	state.Main.LastLine = stream.mark().line;
	recurse(state.Main)

	return state;
end


local function Preprocess(input)
	input = input .. "\n";
	local output = "";
	local definitions = {};

	for whitespace, line in input:gmatch("(%s*)(.-)\n") do
		for i, v in next, definitions, nil do
			line = line:gsub(i, v);
		end

		if line:sub(1, 7) == "#define" then
			local a, b = line:match("#define%s+(%w+)%s(.+)")
			definitions[a] = b;

			output = output .. "\n";

		elseif line:sub(1, 8) == "#include" then
			-- TODO Preprocessor: `#include`
		else
			local depth, type = 0;
			local new = "";

			for ch in line:gmatch(".") do
				if ch == "\"" or ch == "\'" or ch == "[[" then
					if ch == type then
						depth = depth - 1;
						type = nil;
					else
						depth = depth + 1;
						type = ch;
					end
				end

				if ch == ";" and depth == 0 then
					break;
				end

				new = new .. ch;
			end

			output = output .. whitespace .. new .. "\n";
		end
	end

	return output;
end

local function Assemble(input)
	local lasm = Preprocess(input);
	local stream = MakeStream(lasm);
	local object = ParseStream(stream);
	return Link(object);
end

return {Link = Link, Delink = Delink, Assemble = Assemble};
