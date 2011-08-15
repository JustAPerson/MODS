local OpSpecs = {
	"MOVE",
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

	-- opcode = {opcode_number, enum_type, {parameter_types}}
	-- enum_type:
		-- ABC = 0
		-- ABx = 1
		-- AsBx = 2
	-- Parameter types (likely to change):
		-- Unused/Arbitrary = 0
		-- Register 		= 1
		-- Constant 		= 2
		-- Constant/Register= 3
		-- Upvalue 			= 4
		-- Jump Distace 	= 5

	MOVE		= {0, 0,	{1, 1, 0}};
	LOADK		= {1, 1,	{1, 2}};
	LOADBOOL		= {2, 0,	{1, 0, 0}};
	LOADNIL		= {3, 0,	{1, 1, 1}};
	GETUPVAL		= {4, 0,	{1, 4}};
	GETGLOBAL	= {5, 1,	{1, 2}};
	GETTABLE		= {6, 0,	{1, 1, 3}};
	SETGLOBAL	= {7, 1,	{1, 2}};
	SETUPVAL		= {8, 0,	{1, 4}};
	SETTABLE		= {9, 0,	{1, 3, 3}};
	NEWTABLE		= {10, 0,	{1, 0, 0}};
	SELF			= {11, 0,	{1, 1, 3}};
	ADD			= {12, 0,	{1, 1, 3}};
	SUB			= {13, 0,	{1, 1, 3}};
	MUL			= {14, 0,	{1, 1, 3}};
	DIV			= {15, 0,	{1, 1, 3}};
	MOD			= {16, 0,	{1, 1, 3}};
	POW			= {17, 0,	{1, 1, 3}};
	UNM			= {18, 0,	{1, 1, 0}};
	NOT			= {19, 0,	{1, 1, 0}};
	LEN			= {20, 0,	{1, 1, 0}};
	CONCAT		= {21, 0,	{1, 1, 1}};
	JMP			= {22, 2,	{0, 5}};
	EQ			= {23, 0,	{1, 3, 3}};
	LT			= {24, 0,	{1, 3, 3}};
	LE			= {25, 0,	{1, 3, 3}};
	TEST			= {26, 0,	{1, 0, 1}};
	TESTSET		= {27, 0,	{1, 1, 1}};
	CALL			= {28, 0,	{1, 0, 0}};
	TAILCALL		= {29, 0,	{1, 0, 0}};
	RETURN		= {30, 0,	{1, 0, 0}};
	FORLOOP		= {31, 2,	{1, 5}};
	FORPREP		= {32, 2,	{1, 5}};
	TFORLOOP		= {33, 0,	{1, 0}};
	SETLIST		= {34, 0,	{1, 0, 0}};
	CLOSE		= {35, 0,	{1, 0, 0}};
	CLOSURE		= {36, 1,	{1, 0}};
	VARARG		= {37, 0,	{1, 1, 0}};
}

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
	c0 = OpSpecs[op.Opcode] + slb(keep(op.A, 2), 6)
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
		if #s ~= 0 then
			return DumpBinary.Int32(#s+1)..s.."\0"
		else
			return "\0\0\0\0";
		end
	end,
	Integer = function(n)
		return DumpBinary.Int32(n)
	end,
	Int8 = function(n)
		return string.char(n)
	end,
	Int16 = function(n)
		error("DumpBinary::Int16() Not Implemented")
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
		chunk = chunk .. DumpBinary.String(assert(tab.Name, "Invalid Prototype; proto.Name (nil)"));
		chunk = chunk .. DumpBinary.Int32(assert(tab.FirstLine, "Invalid Prototype; proto.FirstLine (nil)"));
		chunk = chunk .. DumpBinary.Int32(assert(tab.LastLine, "Invalid Prototype; proto.LastLine (nil"));
		chunk = chunk .. DumpBinary.Int8(assert(tab.NumberOfUpvalues, "Invalid Prototype; proto.NumberOfUpvalues (nil)"))
		chunk = chunk .. DumpBinary.Int8(assert(tab.Arguments, "Invalid Prototype; proto.Arguments (nil)"))
		chunk = chunk .. DumpBinary.Int8(assert(tab.VargFlag, "Invalid Prototype; proto.VargFlag (nil)"))
		chunk = chunk .. DumpBinary.Int8(assert(tab.MaxStackSize, "Invalid Prototype; proto.MaxStackSize (nil)"))

		chunk = chunk .. DumpBinary.Int32(assert(tab.Instructions.Count, "Invalid Prototype; proto.NumberOfInstructions (nil)"))
		for i=0, #tab.Instructions do
			local ins = tab.Instructions[i];

			chunk = chunk .. OpcodeEncode(ins);
		end

		chunk = chunk .. DumpBinary.Int32(assert(tab.Constants.Count, "Invalid Prototype; proto.NumberOfConstants (nil)"))
		for i=0, #tab.Constants do
			local k = tab.Constants[i];

			if k.ConstantType == "nil" then
				chunk = chunk .. DumpBinary.Int8(0);
			elseif k.ConstantType == "Bool" then
				chunk = chunk .. DumpBinary.Int8(1)
				chunk = chunk .. DumpBinary.Int8(k.Value and 1 or 0)
			elseif k.ConstantType == "Number" then
				chunk = chunk .. DumpBinary.Int8(3)
				chunk = chunk .. DumpBinary.Float64(k.Value)
			elseif k.ConstantType == "String" then
				chunk = chunk .. DumpBinary.Int8(4)
				chunk = chunk .. DumpBinary.String(k.Value)
			end
		end

		chunk = chunk .. DumpBinary.Integer(assert(tab.Protos.Count, "Invalid Prototype; proto.NumberOfProtos (nil)"))
		for i = 0, #tab.Protos do
			recurse(tab.Protos[i])
		end

		chunk = chunk .. DumpBinary.Int32(tab.Instructions.Count)
		for i = 0, #tab.Instructions do
			chunk = chunk .. DumpBinary.Int32(assert(tab.Instructions[i].LineNumber, "Invalid Instruction; instr.LineNumber (nil)"))
		end

		chunk = chunk .. DumpBinary.Int32(assert(tab.Locals.Count, "Invalid Prototype; proto.NumberOfLocals (nil)"));
		for i = 0, #tab.Locals do
			local l = tab.Locals[i];
			chunk = chunk .. DumpBinary.String(assert(l.Name, "Invalid Local; local.Name (nil)"))
			chunk = chunk .. DumpBinary.Int32(assert(l.SPC, "Invalid Local; local.SPC (nil)"))
			chunk = chunk .. DumpBinary.Int32(assert(l.EPC, "Invalid Local; local.EPC (nil)"))
		end

		chunk = chunk .. DumpBinary.Int32(assert(tab.NumberOfUpvalues, "Invalid Prototype; proto.NumberOfUpvalues (nil)"))
		for i = 0, #tab.Upvalues do
			chunk = chunk .. DumpBinary.String(assert(tab.Upvalues[i].Name, "Invalid Upvalue; upval.Name (nil)"));
		end
	end

	chunk = chunk .. '\27Lua\81\0\1\4\4\4\8\0'
	recurse(tab.Main)

	return chunk;
end

local function Delink(chunk)
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
				instr.Opcode =  LuaOpName[opcode+1]
				instr.OpcodeType = LuaOpTypeLookup[opcode+1]
				if instr.OpcodeType == "ABC" then
					instr.A = bit.get(op, 7, 14)
					instr.B = bit.get(op, 24, 32)
					instr.C = bit.get(op, 15, 23)
				elseif instr.OpcodeType == "ABx" then
					instr.A = bit.get(op, 7, 14)
					instr.Bx = bit.get(op, 15, 32)
				elseif instr.OpcodeType == "AsBx" then
					instr.A = bit.get(op, 7, 14)
					instr.sBx = bit.get(op, 15, 32) - 131071
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
					k.ConstantType = "Bool"
					k.Value = GetInt8() ~= 0
				elseif ty == 3 then
					k.ConstantType = "Number"
					k.Value = GetFloat64();
				elseif ty == 4 then
					k.ConstantType = "String"
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

function exception(o)
	error( "["..o.type .. "]:" .. o.pos.line ..":" ..o.pos.char ..": " .. o.msg, 0)
end

function	warn(o)
	print( "[" .. o.type .. "] :" .. o.pos.line ..":" ..o.pos.char..": " .. o.msg)
end

function try(f, e)
	local res = {pcall(f)}
	if not res[1] then
		if type(err) == "table" then
			e(res[2])
		else
			error(err)
		end
	end
end

function lookupify(tb)
	for k, v in pairs(tb) do
		tb[v] = true
	end
	return tb
end

function trueFunc()
	return true
end

function falseFunc()
	return false
end

local isWhiteLookup = {["\n"] = true, ["\t"] = true, [" "] = true}
function isWhite(ch)
	return isWhiteLookup[ch]
end

local isDigitLow = ("0"):byte()
local isDigitHigh = ("9"):byte()
function isDigit(ch)
	local n = ch:byte()
	return n >= isDigitLow and n <= isDigitHigh
end

local isAlphaLow = ("a"):byte()
local isAlphaHigh = ("z"):byte()
function isAlpha(ch)
	local n = ch:lower():byte()
	return (n >= isAlphaLow and n <= isAlphaHigh) or n == 95
end

local directives = {}

--
-- TokenStream class
--
function MakeStream(str)
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
		".macro",
	};
	lookupify(keywords);

	--symbol lookups
	local symbols1 = {
		":", ";", ".", ",", "(", ")", "[", "]", "{", "}", "=", "+", "-", "/", "*", "%";
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
			end

			return true;
		end or falseFunc
		--
		tk.isKeyword = (tk.type == "keyword") and function(kw)
			return kw and tk.data == kw or true
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
			local lookFor = get();
			--string, containing escape characters
			local str = ""
			while not get(lookFor) do
				--unfinished string
				if eof() then
					exception{type="lex", msg="Unfinished string near <eof>", pos=mark()}
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

			if sy1 == "." and alpha(sy2) then	-- directives, gonna call 'em keywords though
				local ident = "." .. sy2;

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
				get()
				pushToken({type="symbol", data=sy1}, tbegin, mark())
			else
				--nothing recognized by lexer
				--exception{type="lex", msg="Bad input character `" .. sy1 .. "`", pos=mark()}
				get()
			end
		end
	end
	pushToken{type="eof"}

	--============================================
	-- access, provide token stream functionality
	--============================================

	local this = {}
	local mTPos = 1
	local mEofToken = tokens[#tokens]

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
			err = err or "Expected `" .. sy .. "`"
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
			err = err or "Expected <number>"
			exception{type="assembler", msg=err, pos=this.mark()}
		end
	end

	return this
end

local directives = {
	[".local"] = function(stream, state)
		local list = {};
		local c = 0;

		while true do
			if stream.isSting() then
				list[#list + 1] = {name = stream.get(), ident = false, type = "local"}
			elseif token.isIdent() then
				list[#list + 1] = {name = stream.get(), ident = true, type = "local"}
			else
				exception{type="assembler", pos=stream.mark(), msg="<identifier / string> expected after `local` directive"};
			end

			if not stream.isSymbol(",") then
				break;
			end
		end

		if stream.consumeSymbol("=") then
			while true do
				c = c + 1;
				list[c].value = stream.expectNumber();

				if not stream.isSymbol(",") then
					break;
				end
			end
		end

		if c ~= #list then
			for i = c + 1, #list do
				list[i].value = state.Current.Locals[0] and #state.Current.Locals or 0;
			end
		end

		for i,v in next, list, nil do
			state.Current:PushIdent(v)

			if not v.ident then

			end
		end
	end,
}; 

local function getExpression(stream)
	
end

function ParseStream(stream)
	local state;
	state = {
		Main = {
			Idents = {};

			Instructions = {};
			Constants = {};
			Protos = {};
			Locals = {};
			Upvalues = {};
		},

		Macros = {};

		NewProto = function(this)
			local a = this.Protos;
			local b = {
				Parent = this;
				Idents = {};

				pc = 0;

				Instructions = {};
				Constants = {};
				Protos = {};
				Locals = {};

				NewProto = state.NewProto;
				PushInstr = state.PushInstr;
				PushIdent = state.PushIdent;
			};

			if a[0] then
				a[#a + 1] = b;
			else
				a[0] = a;
			end

			state.Current = b;

			return b;
		end,

		PushInstr = function(this, int, a, b, c)
			local x = this.Instructions;
			local y = {}

			x.pc = x.pc + 1;

			y.pc = #x
			y.OpcodeType = OpLookup[int].Type;
			y.A = a;
			y.B = b;
			y.C = c;
		end,

		PushIdent = function(this, val)
			local a = this.Idents;

			if a[val.name] then
				warn{pos=stream.mark(), msg = "Overwriting pre-existing Identifer `" .. val.name .. "`"}
			end

			val.pc = this.pc;
			val.start = stream.mark();

			function val.Resolve(this)
				if this.type == "label" then
					return state.Current.pc + 1 - this.pc;
				else
					return this.value;
				end
			end

			a[val.name] = val;
		end,


	}

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
					name = token;
					type = "label";
				};
			else
				-- TODO Token is an Instruction
				
			end
		else
			stream.get();
		end
	end
end

local stream;
if Workspace then
	stream = Workspace["[LuaASM]_MODS_[v2.0.0-pre-alpha]"].example.StringValue.Value;
else
	local file = io.open("example.lasm", "r");
	stream = MakeStream(file:read("*all"));
	file:close();
end
	ParseStream(stream);

--[[
Old code is Old
			elseif stream.consumeSymbol("(") then
				-- TODO REMAKE `MACRO`s
				-- Identifier is a Macro
				local params = {};

				-- gather parameters
				if stream.isIdent() or stream.isNumber() or stream.isString() then
					while true do
						params[#params + 1] = stream.get();

						if stream.consumeSymbol(")") then
							break;
						end
					end
				end

				if stream.consumeSymbol("{") then
					-- We're defining the macro
					local depth = 0;
					local lastline = token.tend.line;
					local loadstr = "return function(state, ";

					for i,v in next, params, nil do
						if not v.isIdent() then
							exception{type="parse", msg="Expected <ident> in macro decleration", pos=this.mark()}
						end

						loadstr = loadstr .. v.data .. (i < #params and "," or ")");
					end

					while true do
						local mini = stream.get(); -- new token
						do
							-- this whole shenanigans is to make sure that if the macro
							-- errors, that the error will be on the same line as where the
							-- macro is located in the lua asm source
							local line = mini.tend.line;

							if line > lastline then
								loadstr = loadstr .. ("\n"):rep(line - lastline);
								lastline = line;
							end

							if mini.data == "{" then
								depth = depth + 1;
							elseif mini.data == "}" then
								if depth == 0 then
									break;
								else
									depth = depth - 1;
								end
							end
						end

						if mini.isString() then
							loadstr = loadstr .. " " .. mini.lookFor .. mini.data .. mini.lookFor;
						else
							loadstr = loadstr .. " " .. mini.data;
						end
					end

					loadstr = loadstr .. "end";

					if state.Macros[token.data] then


					state.Macros[token.data] = loadstring(loadstr)();
				else
					-- We're calling the macro
					for i, v in next, params, nil do
						if v.isIdent() then
							params[i] = state.Current.Idents[v.data];
						else
							params[i] = v.data;
						end
					end

					state.Macros[token.data](state, unpack(params))
				end
 ]]