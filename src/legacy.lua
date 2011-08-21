local p2 = {1,2,4,8,16,32,64,128,256, 512, 1024, 2048, 4096, 8192, 16384, 32768, 65536, 131072}
local function keep (x, n) return x % p2[n+1] end
local function srb (x,n) return math.floor(x / p2[n+1]) end
local function slb (x,n) return x * p2[n+1] end
local LuaOp = {
	MOVE = 0,
	LOADK = 1,
	LOADBOOL = 2,
	LOADNIL = 3,
	GETUPVAL = 4,
	GETGLOBAL = 5,
	GETTABLE = 6,
	SETGLOBAL = 7,
	SETUPVAL = 8,
	SETTABLE = 9,
	NEWTABLE = 10,
	SELF = 11,
	ADD = 12,
	SUB = 13,
	MUL = 14,
	DIV = 15,
	MOD = 16,
	POW = 17,
	UNM = 18,
	NOT = 19,
	LEN = 20,
	CONCAT = 21,
	JMP = 22,
	EQ = 23,
	LT = 24,
	LE = 25,
	TEST = 26,
	TESTSET = 27,
	CALL = 28,
	TAILCALL = 29,
	RETURN = 30,
	FORLOOP = 31,
	FORPREP = 32,
	TFORLOOP = 33,
	SETLIST = 34,
	CLOSE = 35,
	CLOSURE = 36,
	VARARG = 37
}

local CreateOp = {
	ABC = function(op, a, b, c)
		return {TY="ABC", OP=op, A=a, B=b, C=c}
	end,
	ABx = function(op, a, bx)
		return {TY="ABx", OP=op, A=a, Bx=bx}
	end,
	AsBx = function(op, a, sbx)
		return {TY="AsBx", OP=op, A=a, sBx=sbx} -- Bx = sbx
	end,
	Encode = function(op)
		local c0, c1, c2, c3
		if op.sBx then op.Bx = op.sBx + 131071 end
		if op.Bx then op.C = keep(op.Bx, 9); op.B = srb (op.Bx, 9) end
		c0 = op.OP + slb(keep(op.A, 2), 6)
		c1 = srb(op.A, 2) + slb(keep(op.C, 2), 6)
		c2 = srb(op.C, 2) + slb(keep (op.B, 1), 7)
		c3 = srb(op.B, 1)
		return string.char(c0, c1, c2, c3)
	end
}


local DumpBinary;
DumpBinary = {
	String = function(s)
		return DumpBinary.Integer(#s+1)..s.."\0"
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

function CreateChunk()
	local chunk = {}
	-----
	local nargs = 0
	local vConstants = {}
	local nextConstantIndex = 1
	local vCode = {}
	local vProto = {}
	local CNAME = "LuaCXX Source"
	local U,A,V,S = 0, 0, 0, 10
	local UPV, LCL = {}, {}
	-----
	chunk.CompileChunk = function()
		--append header
		return "\027Lua\81\0\1\4\4\4\8\0"..chunk.Compile()
	end
	chunk.Compile = function()
		local body = ""
		body = body..DumpBinary.String(CNAME)
		body = body..DumpBinary.Integer(1) --first line
		body = body..DumpBinary.Integer(1) --last line
		body = body..DumpBinary.Int8(U) --upvalues
		body = body..DumpBinary.Int8(A) --arguments
		body = body..DumpBinary.Int8(V) --VARG_FLAG
		body = body..DumpBinary.Int8(S) --max stack size
		do --instructions
			body = body..DumpBinary.Integer(#vCode+1)
			for i = 1, #vCode do
				body = body..CreateOp.Encode(vCode[i])
			end
			body = body..CreateOp.Encode(CreateOp.ABC(LuaOp.RETURN, 0, 1, 0))
		end
		do --constants
			local const = {}
			for k, v in pairs(vConstants) do
				const[v] = k
			end
			--
			body = body..DumpBinary.Integer(#const)
			for i = 1, #const do
				local c = const[i]
				if type(c) == "string" then
					body = body..DumpBinary.Int8(4)..DumpBinary.String(c)
				elseif type(c) == "number" then
					body = body..DumpBinary.Int8(3)..DumpBinary.Float64(c)
				elseif type(c) == "boolean" then
					body = body..DumpBinary.Int8(1)..DumpBinary.Int8(c and 1 or 0)
				elseif type(c) == "nil" then
					body = body..DumpBinary.Int8(0)
				end
			end
		end
		do --protos
			body = body..DumpBinary.Integer(#vProto)
			for i = 1, #vProto do
				body = body..vProto[i].Compile()
			end
		end
		--add PLENTIFUL debug into
		body = body..DumpBinary.Integer(0) --// 0 line thingies
		body = body..DumpBinary.Integer(#LCL)
		for i,v in pairs(LCL) do
			body = body .. DumpBinary.String(v[1]) .. DumpBinary.Integer(v[2]) .. DumpBinary.Integer(v[3]);
		end
		body = body..DumpBinary.Integer(#UPV)
		for i,v in pairs(UPV) do
			body = body .. DumpBinary.String(v)
		end
		return body
	end
	--
	chunk.GetOp = function(inx)
		return vCode[inx]
	end
	chunk.GetNumOp = function()
		return #vCode
	end
	chunk.PushOp = function(op)
		vCode[#vCode+1] = op
	end
	--
	chunk.GetProto = function(inx)
		return vProto[inx]
	end
	chunk.GetNumProto = function()
		return #vProto
	end
	chunk.PushProto = function(proto)
		vProto[#vProto+1] = proto
	end
	chunk.SetProtoIndex = function(proto, inx)
		vProto[inx] = proto
	end
	--
	chunk.GetConstant = function(constant)
		local inx = vConstants[constant]
		if inx then
			return inx
		else
			inx = nextConstantIndex
			nextConstantIndex = nextConstantIndex + 1
			vConstants[constant] = inx
			return inx
		end
	end
	chunk.SetConstantIndex = function(constant, index) --manually set an index to a constant
		nextConstantIndex = index + 1
		vConstants[constant] = index
	end
	chunk.SetParams = function(u,a,v,s)
		U,A,V,S = u,a,v,s;
	end
	chunk.SetLocals = function(l,u)
		LCL,UPV = l,u
	end
	chunk.SetName = function(str)
		CNAME = tostring(str);
	end
	return chunk
end

function CreateAnObject(ty, callfunc)
	if callfunc then
		return setmetatable({type=ty}, {__call = callfunc})
	else
		return {type=ty}
	end
end

function CheckAValue(fname, a)
	if a < 0 or a > 255 then
		error("Argument #1 to "..fname.." must be in the range [0-255]")
	end
end

function CheckIsConstant(a, err)
	if type(a) ~= "table" or a.type ~= "constant" then
		error(err)
	end
end

function CheckIsProto(a, err)
	if type(a) ~= "table" or a.type ~= "proto" then
		error(err)
	end
end

function CheckIsRK(a, err)
	if type(a) ~= "number" and (type(a) ~= "table" or a.type ~= "constant") then
		error(err)
	end
end

function aChunk(chunkname, LCL,UPV,U,A,V,S, CNAME)
	return CreateAnObject("chunk", function(self, childtb)
		self.name = chunkname
		self.nextconst = 1
		self.const = {} --my constants
		self.code = {}	--instruction : value
		self.nextproto = 1
		self.proto = {} --name : object
		self.pushop = function(op)
			self.code[#self.code+1] = op
		end
		self.buildchunk = function()
			local chunk = CreateChunk()
			--args
			chunk.SetParams(U,A,V,S);
			chunk.SetLocals(LCL, UPV);
			chunk.SetName(CNAME or "LuaCXX Source");
			--constants
			for c, i in pairs(self.const) do
				chunk.SetConstantIndex(c, i)
			end
			--code
			for i = 1, #self.code do
				chunk.PushOp(self.code[i])
			end
			--protos
			for k, p in pairs(self.proto) do
				chunk.SetProtoIndex(p.buildchunk(), p.protonum)
			end
			return chunk
		end
		self.Compile = function()
			return self.buildchunk().CompileChunk()
		end
		--
		for i, child in ipairs(childtb) do
			if child.type == "chunk" then
				child.protonum = self.nextproto
				self.nextproto = self.nextproto + 1
				self.proto[child.name] = child
			elseif child.type == "instruction" then
				child(self)
			end
		end
		--
		return self
	end)
end

function aMOVE(a, b)
	CheckAValue("aMOVE", a)
	CheckAValue("aMOVE", b)
	return CreateAnObject("instruction", function(self, chunk)
		chunk.pushop(CreateOp.ABC(LuaOp.MOVE, a, b, 0))
	end)
end

function aLOADNIL(a, b)
	CheckAValue("aLOADNIL", a)
	CheckAValue("aLOADNIL", b)
	return CreateAnObject("instruction", function(self, chunk)
		chunk.pushop(CreateOp.ABC(LuaOp.LOADNIL, a, b, 0))
	end)
end

function aLOADK(inx, k)
	CheckAValue("aLOADK", inx)
	CheckIsConstant(k, "Argument #2 to aLOADK must be a constant")
	return CreateAnObject("instruction", function(self, chunk)
		chunk.pushop(CreateOp.ABx(LuaOp.LOADK, inx, k(chunk)))
	end)
end

function aLOADBOOL(a, b, c)
	CheckAValue("aLOADBOOL", a)
	CheckAValue("aLOADBOOL", b)
	CheckAValue("aLOADBOOL", c)
	return CreateAnObject("instruction", function(self, chunk)
		chunk.pushop(CreateOp.ABC(LuaOp.LOADBOOL, a, b, c))
	end)
end

function aGETGLOBAL(inx, k)
	CheckAValue("aGETGLOBAL", inx)
	CheckIsConstant(k, "Argument #2 to aGETGLOBAL must be a constant")
	return CreateAnObject("instruction", function(self, chunk)
		chunk.pushop(CreateOp.ABx(LuaOp.GETGLOBAL, inx, k(chunk)))
	end)
end

function aGETUPVAL(a,b)
	CheckAValue("aGETUPVAL",a)
	CheckAValue("aGETUPVAL",b)
	return CreateAnObject("instruction", function(self, chunk)
		chunk.pushop(CreateOp.ABC(LuaOp.GETUPVAL, a, b, 0));
	end)
end

function aSETUPVAL(a,b)
	CheckAValue("aSETUPVAL",a)
	CheckAValue("aSETUPVAL",b)
	return CreateAnObject("instruction", function(self, chunk)
		chunk.pushop(CreateOp.ABC(LuaOp.SETUPVAl, a, b, 0));
	end)
end

function aSETGLOBAL(inx, k)
	CheckAValue("aSETGLOBAL", inx)
	CheckIsConstant(k, "Argument #2 to aGETGLOBAL must be a constant")
	return CreateAnObject("instruction", function(self, chunk)
		chunk.pushop(CreateOp.ABx(LuaOp.SETGLOBAL, inx, k(chunk)))
	end)
end

function aGETTABLE(a, b, c)
	CheckAValue("aGETTABLE", a)
	CheckAValue("aGETTABLE", b)
	CheckIsRK(c, "Argument #3 to aGETTABLE must be a register or constant")
	return CreateAnObject("instruction", function(self, chunk)
		if type(c) == "table" then
			c = c(chunk) + 256
		end
		chunk.pushop(CreateOp.ABC(LuaOp.GETTABLE, a, b, c))
	end)
end

function aSETTABLE(a, b, c)
	CheckAValue("aSETTABLE", a)
	CheckIsRK(b, "Argument #2 to aSETTABLE must be a register or constant")
	CheckIsRK(c, "Argument #3 to aSETTABLE must be a register or constant")
	return CreateAnObject("instruction", function(self, chunk)
		if type(b) == "table" then
			b = b(chunk) + 256
		end
		if type(c) == "table" then
			c = c(chunk) + 256
		end
		chunk.pushop(CreateOp.ABC(LuaOp.SETTABLE, a, b, c))
	end)
end

for _, v in pairs({"ADD", "SUB", "MUL", "DIV", "MOD", "POW", "EQ", "LT", "LE"}) do
	_G["a"..v] = function(a, b, c)
		CheckAValue("a"..v, a)
		CheckIsRK(b, "Argument #2 to a"..v.." must be a register or constant")
		CheckIsRK(c, "Argument #3 to a"..v.." must be a register or constant")
		return CreateAnObject("instruction", function(self, chunk)
			if type(b) == "table" then
				b = b(chunk) + 256
			end
			if type(c) == "table" then
				c = c(chunk) + 256
			end
			chunk.pushop(CreateOp.ABC(LuaOp[v], a, b, c))
		end)
	end
end

function aUNM(a, b)
	CheckAValue("aUNM", a)
	CheckAValue("aUNM", b)
	return CreateAnObject("instruction", function(self, chunk)
		chunk.pushop(CreateOp.ABC(LuaOp.UNM, a, b, 0))
	end)
end

function aNOT(a, b)
	CheckAValue("aNOT", a)
	CheckAValue("aNOT", b)
	return CreateAnObject("instruction", function(self, chunk)
		chunk.pushop(CreateOp.ABC(LuaOp.NOT, a, b, 0))
	end)
end

function aLEN(a, b)
	CheckAValue("aLEN", a)
	CheckAValue("aLEN", b)
	return CreateAnObject("instruction", function(self, chunk)
		chunk.pushop(CreateOp.ABC(LuaOp.LEN, a, b, 0))
	end)
end

function aCONCAT(a, b, c)
	CheckAValue("aCONCAT", a)
	CheckAValue("aCONCAT", b)
	CheckAValue("aCONCAT", c)
	return CreateAnObject("instruction", function(self, chunk)
		chunk.pushop(CreateOp.ABC(LuaOp.CONCAT, a, b, c))
	end)
end

function aJMP(a)
	assert(type(a) == "number", "Argument #1 to aJMP must be a number")
	return CreateAnObject("instruction", function(self, chunk)
		chunk.pushop(CreateOp.AsBx(LuaOp.JMP,0,a))
	end)
end

function aCALL(a, b, c)
	CheckAValue("aCALL", a)
	CheckAValue("aCALL", b)
	CheckAValue("aCALL", c)
	return CreateAnObject("instruction", function(self, chunk)
		chunk.pushop(CreateOp.ABC(LuaOp.CALL, a, b, c))
	end)
end

function aRETURN(a, b)
	CheckAValue("aRETURN", a)
	CheckAValue("aRETURN", b)
	return CreateAnObject("instruction", function(self, chunk)
		chunk.pushop(CreateOp.ABC(LuaOp.RETURN, a, b, 0))
	end)
end

function aTAILCALL(a, b, c)
	CheckAValue("aTAILCALL", a)
	CheckAValue("aTAILCALL", b)
	CheckAValue("aTAILCALL", c)
	return CreateAnObject("instruction", function(self, chunk)
		chunk.pushop(CreateOp.ABC(LuaOp.TAILCALL, a, b, c))
	end)
end

function aVARARG(a, b)
	CheckAValue("aVARARG", a)
	CheckAValue("aVARARG", b)
	return CreateAnObject("instruction", function(self, chunk)
		chunk.pushop(CreateOp.ABC(LuaOp.VARARG, a, b, 0))
	end)
end

function aSELF(a, b, c)
	CheckAValue("aSELF", a)
	CheckAValue("aSELF", b)
	CheckIsRK(c, "Argument #3 to aSELF must be a register or constant")
	return CreateAnObject("instruction", function(self, chunk)
		if type(c) == "table" then
			c = c(chunk) + 256
		end
		chunk.pushop(CreateOp.ABC(LuaOp.SELF, a, b, c))
	end)
end

function aTEST(a, c)
	CheckAValue("aTEST", a)
	CheckAValue("aTEST", c)
	return CreateAnObject("instruction", function(self, chunk)
		chunk.pushop(CreateOp.ABC(LuaOp.TEST, a, 0, c))
	end)
end

function aTESTSET(a, b, c)
	CheckAValue("aTESTSET", a)
	CheckAValue("aTESTSET", b)
	CheckAValue("aTESTSET", c)
	return CreateAnObject("instruction", function(self, chunk)
		chunk.pushop(CreateOp.ABC(LuaOp.TESTSET, a, b, c))
	end)
end

function aFORPREP(a, b)
	CheckAValue("aFORPREP", a)
	return CreateAnObject("instruction", function(self, chunk)
		chunk.pushop(CreateOp.AsBx(LuaOp.FORPREP, a, b))
	end)
end

function aFORLOOP(a, b)
	CheckAValue("aFORLOOP", a)
	return CreateAnObject("instruction", function(self, chunk)
		chunk.pushop(CreateOp.AsBx(LuaOp.FORLOOP, a, b))
	end)
end

function aTFORLOOP(a, c)
	CheckAValue("aTFORLOOP", a)
	CheckAValue("aTFORLOOP", c)
	return CreateAnObject("instruction", function(self, chunk)
		chunk.pushop(CreateOp.ABC(LuaOp.TFORLOOP, a, 0, c))
	end)
end

function aNEWTABLE(a, b, c)
	CheckAValue("aNEWTABLE", a)
	CheckAValue("aNEWTABLE", b)
	CheckAValue("aNEWTABLE", c)
	return CreateAnObject("instruction", function(self, chunk)
		chunk.pushop(CreateOp.ABC(LuaOp.NEWTABLE, a, b, c))
	end)
end

function aSETLIST(a, b, c)
	CheckAValue("aSETLIST", a)
	CheckAValue("aSETLIST", b)
	CheckAValue("aSETLIST", c)
	return CreateAnObject("instruction", function(self, chunk)
		chunk.pushop(CreateOp.ABC(LuaOp.SETLIST, a, b, c))
	end)
end

function aCLOSURE(a, proto)
	CheckAValue("aCLOSURE", a)
	--CheckIsProto(proto, "Argument #2 to aCLOSURE must be a function prototype")
	return CreateAnObject("instruction", function(self, chunk)
		chunk.pushop(CreateOp.ABx(LuaOp.CLOSURE, a, proto))
	end)
end

function aCLOSE(a)
	CheckAValue("aCLOSE", a)
	return CreateAnObject("instruction", function(self, chunk)
		chunk.pushop(CreateOp.ABC(LuaOp.CLOSE, a, 0, 0))
	end)
end

function aK(constant)
	return CreateAnObject("constant", function(self, chunk)
		local constinx = chunk.const[constant]
		if not constinx then
			constinx = chunk.nextconst
			chunk.nextconst = chunk.nextconst + 1
			chunk.const[constant] = constinx
		end
		return constinx-1
	end)
end

function aProto(name)
	return CreateAnObject("proto", function(self, chunk)
		local proto = chunk.proto[name]
		if not proto then
			error("Chunk `"..chunk.name.."` has no prototype named `"..name)
		end
		return proto.protonum-1
	end)
end


local EncodeOp = {
iABC = function(op, a, b, c)

end,
iABx = function(op, a, bx)

end,
iAsBx = function(op, a, sbx)

end
}

local LuaOpName = {
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
"VARARG"
}

local LuaOpType = {
iABC = 0,
iABx = 1,
iAsBx = 2
}

local LuaOpTypeLookup = {
LuaOpType.iABC,
LuaOpType.iABx,
LuaOpType.iABC,
LuaOpType.iABC,
LuaOpType.iABC,
LuaOpType.iABx,
LuaOpType.iABC,
LuaOpType.iABx,
LuaOpType.iABC,
LuaOpType.iABC,
LuaOpType.iABC,
LuaOpType.iABC, --self = ?
LuaOpType.iABC,
LuaOpType.iABC,
LuaOpType.iABC,
LuaOpType.iABC,
LuaOpType.iABC,
LuaOpType.iABC,
LuaOpType.iABC,
LuaOpType.iABC,
LuaOpType.iABC,
LuaOpType.iABC,
LuaOpType.iAsBx,
LuaOpType.iABC,
LuaOpType.iABC,
LuaOpType.iABC,
LuaOpType.iABC,
LuaOpType.iABC,
LuaOpType.iABC,
LuaOpType.iABC,
LuaOpType.iABC,
LuaOpType.iAsBx,
LuaOpType.iAsBx,
LuaOpType.iABC,
LuaOpType.iABC,
LuaOpType.iABC,
LuaOpType.iABx,
LuaOpType.iABC
}

function GetOpString(op)
	local opcode = GetOpCode(op)
	local ty = GetOpCodeType(opcode)
	local name = GetOpCodeName(opcode)
	if ty == LuaOpType.iABC then
		local b = GetOpB(op)
		if b >= 256 then b = "256+"..(b-256) end
		local c = GetOpC(op)
		if c >= 256 then c = "256+"..(c-256) end
		return "OP="..name.." A="..GetOpA(op).." B="..b.." C="..c
	elseif ty == LuaOpType.iABx then
		return "OP="..name.." A="..GetOpA(op).." Bx="..GetOpBx(op)
	elseif ty == LuaOpType.iAsBx then
		return "OP="..name.." A="..GetOpA(op).." sBx="..GetOpsBx(op)
	end
	return "OP=<Bad Type>"
end
function GetOpCodeType(opcode)
	return LuaOpTypeLookup[opcode+1]
end
function GetOpCodeName(opcode)
	return LuaOpName[opcode+1]
end
function GetOpCode(op)
	return bit.get(op, 1, 6)
end
function GetOpA(op)
	return bit.get(op, 7, 14)
end
function GetOpB(op)
	return bit.get(op, 24, 32)
end
function GetOpC(op)
	return bit.get(op, 15, 23)
end
function GetOpBx(op)
	return bit.get(op, 15, 32)
end
function GetOpsBx(op)
	return GetOpBx(op) - 131071
end

function DumpChunk(chunk, indent)
	local indent = indent or 0
	local index = 1
	local big_endian = false
	------------------------------------
	local function print(...)
		print(string.rep("_", indent*2) .. ...)
	end
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
		--[[print("A="..a.." B'"..b)
		print("Bits="..bit.getstring(b, 32).."_"..bit.getstring(a, 32))
		print("Bits="..bit.get(b, 32).."__"..bit.getstring(bit.get(b, 21, 31), 11).."__"..bit.getstring(bit.get(b, 1, 20), 20).."-"..bit.getstring(a, 32))
		print("Sign="..sign..", Exp="..exponent..", Frac="..fraction)]]
		--[[local sign = -2*bit.get(b, 32)+1
		local exponent = bit.get(b, 21, 31)-1023
		local fraction = (bit.get(b, 1, 20)*(2^32) + a)/(2^52)+1
		return sign*(2^exponent)*fraction]]
		local a = GetInt32()
		local b = GetInt32()
		return (-2*bit.get(b, 32)+1)*(2^(bit.get(b, 21, 31)-1023))*((bit.get(b, 1, 20)*(2^32) + a)/(2^52)+1)
	end
	local function GetString(len)
		local str = chunk:sub(index, index+len-1)
		index = index + len
		return str
	end
	------------------------------------
	local function GetTypeInt()
		local a = GetInt8()
		local b = GetInt8()
		local c = GetInt8()
		local d = GetInt8()
		return d*16777216 + c*65536 + b*256 + a
	end
	local function GetTypeString()
		local tmp = GetInt32(str, index)
		return GetString(tmp)
	end
	local function GetTypeFunction()
		print("====FUNCTION DEF====")
		print("Source Name \""..GetTypeString():sub(1, -2).."\", Lines "..GetTypeInt().." to "..GetTypeInt())
		print("Upvalues : "..GetInt8()..", Arguments : "..GetInt8()..", VargFlag : "..GetInt8()..", Max Stack Size : "..GetInt8())
		do
			local num = GetInt32()
			print("Instructions : "..num.." {")
			indent = indent + 1
			for i = 1, num do
				local op = GetInt32()
				local opcode = GetOpCode(op)
				print(GetOpString(op))
			end
			indent = indent - 1
			print("}")
		end
		do
			local num = GetInt32()
			if num > 0 then
				print("Constants : "..num.." {")
				indent = indent + 1
				for i = 1, num do
					local ty = GetInt8()
					if ty == 0 then
						print((i-1).." : NIL")
					elseif ty == 1 then
						print((i-1).." : BOOL = "..(GetInt8() == 0 and "false" or "true"))
					elseif ty == 3 then
						print((i-1).." : NUMBER = "..GetFloat64())
					elseif ty == 4 then
						print((i-1).." : STRING = \""..GetTypeString():sub(1, -2).."\"")
					end
				end
				indent = indent - 1
				print("}")
			else
				print("Constants : 0 {}")
			end
		end
		do
			local num = GetInt32()
			if num > 0 then
				print("Functions Protos : "..num.." {")
				indent = indent + 1
				for i = 1, num do
					GetTypeFunction()
				end
				indent = indent - 1
				print("}")
			else
				print("Function Protos : 0 {}")
			end
		end
		do
			print("====DEBUG INFO====")
			--strip debug info
			local numsrc = GetInt32()
			for i = 1, numsrc do GetInt32() end
			local numlocal = GetInt32()
			print("Local Values : " .. (numlocal > 0 and numlocal .. " {" or "0 {}"))
			indent = indent + 1
			for i = 1, numlocal do
				local name = GetTypeString()
				local spc = GetInt32()
				local epc = GetInt32()
				print("NAME="..name:sub(1,#name-1).." STARTPC="..spc.." ENDPC="..epc)
			end
			indent = indent - 1
			if numlocal > 0 then print("}") end
			local numups = GetInt32()
			print("UpValues : " .. (numups > 0 and numups.. " {" or "0 {}"))
			indent = indent + 1
			for i = 1, numups do print("NAME="..GetTypeString()) end
			indent = indent - 1
			if numups > 0 then print("}") end

		end
	end
	---------------------------------------
	--get chunk start
	print("====HEADER====")
	print("Chunk Identifier : "..GetString(4))
	print("Version number : "..GetInt8())
	print("Format : "..((GetInt8() == 0) and "Official" or "Unofficial"))
	big_endian = (GetInt8() == 0)
	print("Format version : "..(big_endian and "Big Endian (0)" or "Little Endian (1)"))
	print("Size of int : "..GetInt8().." bytes")
	print("Size of size_t : "..GetInt8().." bytes")
	print("Size of instruction : "..GetInt8().." bytes")
	print("Size of lua_Number : "..GetInt8().." bytes")
	print("Number format : "..((GetInt8() == 0) and "FP" or "INT"))
	GetTypeFunction()
end

local NPROTO = 0;

local function base(str, name, NUPS, NARGS, VFLAG, STACKS, CNAME)
	local S = "";
    for line in str:gmatch("(.-)%\n") do     --// helps with allowing tabbing.
        S = S .. " \t" .. line .. " \n";
    end
    str = S .. " ";
    local S;

    --// Variables
    local POS = 0;
    local CONSTLIST = {}
    local functSTART = 0;
    local functCODE = "";
    local OPCODES = {};
	local LOCALS = {}
	local UPVALS = {}
    NUPS, NARGS, VFLAG, STACKS = NUPS or 0, NARGS or 0, VFLAG or 0, STACKS or 10

    --// Main
    for line in str:gmatch("%s+(.-)%\n") do
	POS = POS + 1
    local forgetThis, useThis = functCODE:gsub("\n","");
		if POS > functSTART + useThis then
			if line:sub(1,1) ~= ";" then
				if line:sub(1,1) == "." then                    --// directives
					local op = line:match("(.%a-)%s")
					local para = line:sub(#op+2)
					if op:lower() == ".const" then
						local _, k = (para .. ";"):match("([\'\"])(.-)%1;")
						if not k and not _ then _, k= (para .. ";"):match("([\'\"])(.-)%1%s-;") end
						if not k then _,k = "", (para .. ";"):match("(%d+)%s-;") end
						local c = loadstring("return aK(" .. _ .. k .. _ .. ")")();
						table.insert(CONSTLIST, c);
					elseif op:lower() == ".local" then
						local str, s, e = para:match("%s?(.+)%s(%d+)%s(%d+)")
						if not str then
							local _, k = (para .. ";"):match("([\'\"])(.-)%1;")
							if not k and not _ then _, k= (para .. ";"):match("([\'\"])(.-)%1%s-;") end
							str, s, e = _..k.._, 0, 0
						end
						str,s,e = loadstring("return " .. str)(), tonumber(s), tonumber(e)
						table.insert(LOCALS, {str,s,e})
					elseif op:lower() == ".funct" then
						local upv, args, var, maxs = line:sub(line:find(op)+#op+1):match("%s?(%d+)%s(%d+)%s(%d+)%s(%d+)");
						upv,args,var,maxs = tonumber(upv),tonumber(args),tonumber(var),tonumber(maxs)
						local depth = 1;
						functSTART = POS;
						functCODE = "";
						local keepGoing = 1;
						local fCODE = "";
						local currentNPROTO = NPROTO;

						for miniLine in str:gmatch("%s+(.-)%\n") do
							if keepGoing > functSTART then
								fCODE = fCODE .. miniLine .. "\n";
							end
							keepGoing = keepGoing + 1;
						end

						for miniLine in fCODE:gmatch("(.-)%\n") do
							if miniLine:sub(1,6) == ".funct" then
								depth = depth + 1;
							elseif miniLine:sub(1,4) == ".end" then
								depth = depth - 1;
							end
							if depth > 0 then
								functCODE = functCODE .. miniLine .. "\n";
							else break; end
						end

						table.insert(OPCODES, base(functCODE,"PROTO"..currentNPROTO, upv, args, var, maxs))
						aProto("PROTO"..currentNPROTO)
						NPROTO = NPROTO + 1;
					elseif op:lower() == ".upval" then
						local _, k = (para .. ";"):match("([\'\"])(.-)%1;")
						if not k and not _ then _, k= (para .. ";"):match("([\'\"])(.-)%1%s-;") end
						if not k then _,k = "", (para .. ";"):match("(%d+)%s-;") end
						table.insert(UPVALS, loadstring("return " .._..k.._)())
					elseif op:lower() == ".optns" then
						local upv, args, var, maxs = line:sub(line:find(op)+#op+1):match("%s?(%d+)%s(%d+)%s(%d+)%s(%d+)");
						upv,args,var,maxs = tonumber(upv),tonumber(args),tonumber(var),tonumber(maxs)
						NUPS, NARGS, VFLAG, STACKS = upv,args,var,maxs
					elseif op:lower() == ".cname" then
						local _, k = (para .. ";"):match("([\'\"])(.-)%1")
						if not k and not _ then _, k= (para .. ";"):match("([\'\"])(.-)%1%s-;") end
						if not k then _,k = "", (para .. ";"):match("(%d+)%s-;") end
						CNAME = loadstring("return " .. _ .. k .. _)();
					end
				else                                       --// opcodes
					local OPTIONS = {}
					local opcode = line:match("(%a+)%s")
					if opcode then
						for option in line:gmatch("(.%d+)") do
							if option:sub(1,1):lower() == "k" then
								table.insert(OPTIONS, CONSTLIST[tonumber(option:sub(2)) + 1]);
							else
								table.insert(OPTIONS, tonumber(option));
							end
						end
						table.insert(OPCODES, getfenv()["a" .. opcode:upper()](unpack(OPTIONS)));
					end
				end
			end
		end
    end
    local chunk = aChunk(name or "MAIN", LOCALS, UPVALS, NUPS, NARGS, VFLAG, STACKS, CNAME)(OPCODES);
    return chunk;
end

function parseLASM(str)
    local chunk = base(str);
    return chunk.Compile();
end

 local LuaOpName = {
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
"VARARG"
}

local LuaOpType = {
iABC = "ABC";
iABx = "ABx";
iAsBx = "AsBx";
}

local LuaOpTypeLookup = {
LuaOpType.iABC,
LuaOpType.iABx,
LuaOpType.iABC,
LuaOpType.iABC,
LuaOpType.iABC,
LuaOpType.iABx,
LuaOpType.iABC,
LuaOpType.iABx,
LuaOpType.iABC,
LuaOpType.iABC,
LuaOpType.iABC,
LuaOpType.iABC, --self = xLEGOx's Question (ABC works)
LuaOpType.iABC,
LuaOpType.iABC,
LuaOpType.iABC,
LuaOpType.iABC,
LuaOpType.iABC,
LuaOpType.iABC,
LuaOpType.iABC,
LuaOpType.iABC,
LuaOpType.iABC,
LuaOpType.iABC,
LuaOpType.iAsBx,
LuaOpType.iABC,
LuaOpType.iABC,
LuaOpType.iABC,
LuaOpType.iABC,
LuaOpType.iABC,
LuaOpType.iABC,
LuaOpType.iABC,
LuaOpType.iABC,
LuaOpType.iAsBx,
LuaOpType.iAsBx,
LuaOpType.iABC,
LuaOpType.iABC,
LuaOpType.iABC,
LuaOpType.iABx,
LuaOpType.iABC
}

local LuaOpcodeParams = {
["MOVE"] = {1, 1, 5};
["LOADK"] = {1, 2};
["LOADBOOL"] = {1, 0, 0};
["LOADNIL"] = {1, 1, 1};
["GETUPVAL"] = {1, 4};
["GETGLOBAL"] = {1, 2};
["GETTABLE"] = {1, 1, 3};
["SETGLOBAL"] = {1, 2};
["SETUPVAL"] = {1, 4};
["SETTABLE"] = {1, 3, 3};
["NEWTABLE"] = {1, 0, 0};
["SELF"] = {1, 1, 3};
["ADD"] = {1, 1, 3};
["SUB"] = {1, 1, 3};
["MUL"] = {1, 1, 3};
["DIV"] = {1, 1, 3};
["MOD"] = {1, 1, 3};
["POW"] = {1, 1, 3}; -- forgot to add this till 7/14/11
["UNM"] = {1, 1, 5};
["NOT"] = {1, 1, 5};
["LEN"] = {1, 1, 5};
["CONCAT"] = {1, 1, 1};
["JMP"] = {0, 5};
["EQ"] = {1, 3, 3};
["LT"] = {1, 3, 3};
["LE"] = {1, 3, 3};
["TEST"] = {1, 0, 1};
["TESTSET"] = {1, 1, 1};
["CALL"] = {1, 0, 0};
["TAILCALL"] = {1, 0, 0};
["RETURN"] = {1, 0, 5};
["FORLOOP"] = {1, 5};
["FORPREP"] = {1, 5};
["TFORLOOP"] = {1, 0};
["SETLIST"] = {1, 0, 0};
["CLOSE"] = {1, 5, 5};
["CLOSURE"] = {1, 0};
["VARARG"] = {1, 1, 5}
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
local LuaOp = {
	MOVE = 0,
	LOADK = 1,
	LOADBOOL = 2,
	LOADNIL = 3,
	GETUPVAL = 4,
	GETGLOBAL = 5,
	GETTABLE = 6,
	SETGLOBAL = 7,
	SETUPVAL = 8,
	SETTABLE = 9,
	NEWTABLE = 10,
	SELF = 11,
	ADD = 12,
	SUB = 13,
	MUL = 14,
	DIV = 15,
	MOD = 16,
	POW = 17,
	UNM = 18,
	NOT = 19,
	LEN = 20,
	CONCAT = 21,
	JMP = 22,
	EQ = 23,
	LT = 24,
	LE = 25,
	TEST = 26,
	TESTSET = 27,
	CALL = 28,
	TAILCALL = 29,
	RETURN = 30,
	FORLOOP = 31,
	FORPREP = 32,
	TFORLOOP = 33,
	SETLIST = 34,
	CLOSE = 35,
	CLOSURE = 36,
	VARARG = 37
}

local OpcodeEncode = function(op)
	local c0, c1, c2, c3
	if op.OpcodeType == "AsBx" then op.Bx = op.sBx + 131071 op.OpcodeType = "ABx" end
	if op.OpcodeType == "ABx" then op.C = keep(op.Bx, 9); op.B = srb (op.Bx, 9) end
	c0 = LuaOp[op.Opcode] + slb(keep(op.A, 2), 6)
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
		return DumpBinary.Int32(#s+1)..s.."\0"
	end,
    SpecString = function(s)
		return #s == 0 and "\0\0\0\0" or DumpBinary.Int32(#s+1) .. s .. "\0";
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

local function Disassemble(chunk)
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


local function Assemble(tab)
	local chunk = "";
	local function recurse(tab)
		chunk = chunk .. DumpBinary.SpecString(assert(tab.Name, "Invalid Prototype; proto.Name (nil)"));
		chunk = chunk .. DumpBinary.Int32(assert(tab.FirstLine, "Invalid Prototype; proto.FirstLine (nil)"));
		chunk = chunk .. DumpBinary.Int32(assert(tab.LastLine, "Invalid Prototype; proto.LastLine (nil"));
		chunk = chunk .. DumpBinary.Int8(assert(tab.NumberOfUpvalues, "Invalid Prototype; proto.NumberOfUpvalues (nil)"))
		chunk = chunk .. DumpBinary.Int8(assert(tab.Arguments, "Invalid Prototype; proto.Arguments (nil)"))
		chunk = chunk .. DumpBinary.Int8(assert(tab.VargFlag, "Invalid Prototype; proto.VargFlag (nil)"))
		chunk = chunk .. DumpBinary.Int8(assert(tab.MaxStackSize, "Invalid Prototype; proto.MaxStackSize (nil)"))

		chunk = chunk .. DumpBinary.Int32(assert(tab.NumberOfInstructions, "Invalid Prototype; proto.NumberOfInstructions (nil)"))
		for i=1, tab.NumberOfInstructions do
			local ins = tab.Instructions[i-1]
			OpcodeChecks[ins.Opcode](tab, ins)
			chunk = chunk .. OpcodeEncode(ins)
		end
		chunk = chunk .. DumpBinary.Int32(assert(tab.NumberOfConstants, "Invalid Prototype; proto.NumberOfConstants (nil)"))
		for i=1, tab.NumberOfConstants do
			local k = tab.Constants[i-1]
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
		chunk = chunk .. DumpBinary.Integer(assert(tab.NumberOfProtos, "Invalid Prototype; proto.NumberOfProtos (nil)"))
		for i=1, tab.NumberOfProtos do
			recurse(tab.Protos[i-1])
		end

		chunk = chunk .. DumpBinary.Int32(tab.NumberOfInstructions)
		for i=1, tab.NumberOfInstructions do
			chunk = chunk .. DumpBinary.Int32(assert(tab.Instructions[i-1].LineNumber, "Invalid Instruction; instr.LineNumber (nil)"))
		end

		chunk = chunk .. DumpBinary.Int32(assert(tab.NumberOfLocals, "Invalid Prototype; proto.NumberOfLocals (nil)"));
		for i=1, tab.NumberOfLocals do
			local l = tab.Locals[i-1];
			chunk = chunk .. DumpBinary.String(assert(l.Name, "Invalid Local; local.Name (nil)"))
			chunk = chunk .. DumpBinary.Int32(assert(l.SPC, "Invalid Local; local.SPC (nil)"))
			chunk = chunk .. DumpBinary.Int32(assert(l.EPC, "Invalid Local; local.EPC (nil)"))
		end

		chunk = chunk .. DumpBinary.Int32(assert(tab.NumberOfUpvalues, "Invalid Prototype; proto.NumberOfUpvalues (nil)"))
		for i=1, tab.NumberOfUpvalues do
			chunk = chunk .. DumpBinary.String(assert(tab.Upvalues[i-1].Name, "Invalid Upvalue; upval.Name (nil)"));
		end
	end

	chunk = chunk .. '\27Lua\81\0\1\4\4\4\8\0'
	recurse(tab.Main)

	return chunk;
end

local LuaOpNToP = {
	["ABC"] = {
		"A",
		"B",
		"C",
	},
	["ABx"] = {
		"A",
		"Bx",
	},
	["AsBx"] = {
		"A",
		"sBx",
	}
}

local function Debug(bytecode, parsable)
	local chunk = Disassemble(bytecode);
	local indent;

	if parsable == true then
		local output = "";
		indent = -1;

		local function log(...)
		    local s = tostring(arg[1])

			for i = 2, #arg do
				s = s .. " " .. tostring(arg[i]);
			end

			output = output ..("\t"):rep(indent) .. s .. "\n";
		end

		local function Params(v, i)
			local n = v[LuaOpNToP[v.OpcodeType][i]];
			local temp = LuaOpcodeParams[v.Opcode][i];

			if temp == 2 then
				return "k" .. n;
			elseif temp == 3 then
				if n > 255 then
					return "k" .. (n - 256);
				else
					return n;
				end
			else
				return n;
			end
		end

		local function parseProto(proto, isProto)
			if isProto then
				log(".funct\t;\tnumber " .. isProto)
			end

			indent = indent + 1;

			log(".cname  '" .. proto.Name .."'");				-- encode the function name
			log(".optns " .. proto.NumberOfUpvalues .. " " .. proto.Arguments .. " "
					.. proto.VargFlag .. " " .. proto.MaxStackSize)		-- encode some parameters

			log("");

			-- encode upvalues
			if proto.Upvalues.Count > 0 then
				for i = 0, #proto.Upvalues do
					log(".upval '" .. proto.Upvalues[i].Name .. "\t\t\t;\t" .. i);
				end

				log("");
			end

			if #proto.Upvalues ~= 0 then log(""); end

			-- encode locals
			if proto.Locals.Count > 0 then
				for i = 0, #proto.Locals do
					local v = proto.Locals[i];

					log(".local '" .. v.Name .. "' " .. v.SPC .. " " .. v.EPC .. "\t\t\t;\t" .. i);
				end

				log("");
			end

			-- encode constants
			if proto.Constants.Count > 0 then
				for i = 0, #proto.Constants do
					local v = proto.Constants[i];

					log(".const " .. (v.ConstantType == "String" and  "'" .. v.Value .. "'" or v.Value)
						.. "\t\t\t;\tk" .. i);
				end

				log("");
			end


			if proto.Protos.Count > 0 then
				for i = 0, #proto.Protos do
					parseProto(proto.Protos[i], i);
				end
			end

			if proto.Instructions.Count > 0 then
				for i = 0, #proto.Instructions do
					local v = proto.Instructions[i];
					local s = v.Opcode;

					if v.OpcodeType == "ABC" then
						s = s .. " " .. Params(v, 1) .. " " .. Params(v, 2) .. " " .. Params(v, 3);
					else
						s = s .. " " .. Params(v, 1) .. " " .. Params(v, 2);
					end

					log(s)
				end
			end

			indent = indent - 1;

			if isProto then
				log(".end")
				log("");
			end
		end

		parseProto(chunk.Main, false);

		return output;
	else
		local xprint = print;
		indent = 1;

		local function print(n, ...)
			xprint(("\t"):rep(n) .. table.concat({...}, ""));
		end

		local function getSpec(chunk, instr, a, b, c)
			local rules = LuaOpcodeParams[instr.Opcode];
			if rules[b] == 1 then
				if a < chunk.NumberOfLocals then
					if chunk.Locals[a].SPC < c and chunk.Locals[a].EPC >= c then
						return "[" .. chunk.Locals[a].Name .."]"
					elseif chunk.Locals[a].SPC == c then
						return "![" .. chunk.Locals[a].Name .."]"
					end
				end
			elseif rules[b] == 2 then
					local k= chunk.Constants[a];
					if k.ConstantType == "String" then
						return "(\'" ..k.Value  .."\')"
					elseif k.ConstantType == "nil" then
						return "(nil)"
					else
						return "("..tostring(k.Value)..")";
					end
			elseif rules[b] == 3 then
				if a >= 256 then
					local k= chunk.Constants[a-256];
					if k.ConstantType == "String" then
						return "(\'" ..k.Value  .."\')"
					elseif k.ConstantType == "nil" then
						return "(nil)"
					else
						return "("..tostring(k.Value)..")";
					end
				else
					if a < chunk.NumberOfLocals then
						if chunk.Locals[a].SPC <= c and chunk.Locals[a].EPC >= c then
							return "[" .. chunk.Locals[a].Name .."]"
						end
					end
				end
			elseif rules[b] == 4 then
				return "<"..(chunk.Upvalues[a] and chunk.Upvalues[a].Name or "")..">";
			elseif rules[b] == 5 then
				return "to [" .. c+a+1 .. "]";
			end

			return a;
		end

		local function printInstructions(indent, chunk)
			print(indent-1, "[Instructions] \tCount: ",chunk.Instructions.Count)
			for i=1, chunk.Instructions.Count do
				local instr = chunk.Instructions[i-1]

				if instr.OpcodeType == "ABC" then
					local a, b, c = instr.A, instr.B, instr.C
					local _a = getSpec(chunk, instr, a, 1, i)
					local _b = getSpec(chunk, instr, b, 2, i)
					local _c = getSpec(chunk, instr, c, 3, i)

					print(indent,"[",i,"] (",instr.LineNumber or 0,")\tOpcode: ", instr.Opcode,#instr.Opcode==4 and "\t\t" or #instr.Opcode< 4  and "\t\t" or "\t",
					a,"\t",b,"\t",c,"\t; ",_a,"\t ",_b,"\t ",_c)
				elseif instr.OpcodeType == "ABx" then
					local a, bx = instr.A, instr.Bx;
					local _a = getSpec(chunk, instr, a, 1, i)
					local _bx = getSpec(chunk, instr, bx, 2, i)

					print(indent,"[",i,"] (",instr.LineNumber or 0,")\tOpcode: ",instr.Opcode,#instr.Opcode == 5 and "\t\t" or #instr.Opcode<=4 and "\t\t\t" or "\t",a,"\t",bx,
					"\t\t; ",_a,"\t ",_bx)
				elseif instr.OpcodeType == "AsBx" then
					local a, sbx = instr.A, instr.sBx;
					local _a, _sbx = getSpec(chunk, instr, a, 1, i), getSpec(chunk, instr, sbx, 2, i);
					if #instr.Opcode == 7 then
						print(indent,"[",i,"] (",instr.LineNumber or 0,")\tOpcode: ",instr.Opcode,"\t",a,"\t",sbx,"\t\t; ",_a,"\t",_sbx)
					else
						print(indent,"[",i,"] (",instr.LineNumber or 0,")\tOpcode: ",instr.Opcode,"\t\t",a,"\t",sbx,"\t\t; ",_a,"\t",_sbx)
					end
				end
			end
		end

		local function printConstants(indent, chunk)
			print(indent-1, "[Constants] \tCount: ", chunk.Constants.Count)
			for i=0, chunk.Constants.Count-1 do
				local k = chunk.Constants[i];
				if k.ConstantType == "String" then
					print(indent,"[",i,"]\tType: (",k.ConstantType,")\t\"",k.Value,"\"")
				else
					print(indent,"[",i,"]\tType: (",k.ConstantType,")\t",tostring(k.Value))
				end
			end
		end

		local function printLocals(indent, chunk)
			print(indent-1, "[Locals]\t\tCount: ", chunk.Locals.Count)
			for i=0, chunk.Locals.Count-1 do
				local l = chunk.Locals[i];
				print(indent,"[",i,"]\t","SPC:\t",l.SPC,"\tEPC:\t",l.EPC,"\tName: \"", l.Name,"\"")
			end
		end

		local function printUpvalues(indent, chunk)
			print(indent-1, "[Upvalues]\tCount:", chunk.Upvalues.Count)
			for i=0, chunk.Upvalues.Count-1 do
				print(indent,"[",i,"]\tName: \"",chunk.Upvalues[i].Name,"\"")
			end
		end

		local function printProto(indent, proto)
			print(indent, "Name: \"", proto.Name, "\"\tLines: ",proto.FirstLine, "-", proto.LastLine)
			print(indent, "Upvalues: ", proto.Upvalues.Count, "\tArguments: ", proto.Arguments, "\tVargFlag: ", proto.VargFlag, "\tMaxStackSize: ", proto.MaxStackSize)
			printInstructions(indent+1, proto)
			printConstants(indent+1, proto)
			printLocals(indent+1, proto)
			printUpvalues(indent+1, proto)

			print(indent, "[Prototypes]\tCount: ", proto.Protos.Count)
			for i=0, proto.Protos.Count-1 do
				print(indent,"[", i,"]")
				printProto(indent+1, proto.Protos[i])
			end
		end

		print(0, "LuaDbg.lua")
		print(0, "")
		print(0, "[Chunk Header]")
		print(1, "Identifier: ", chunk.Identifier)
		print(1, "Version: ", ("0x%02X"):format(chunk.Version))
		print(1, "Format: ", chunk.Format)
		print(1, "Endianness: ", chunk.BigEndian and "Big" or "Litte")
		print(1, "Integer Size: ", chunk.IntSize," bytes")
		print(1, "String Size Size: ", chunk.SizeT," bytes")
		print(1, "Instruction Size: ", chunk.InstructionSize," bytes")
		print(1, "Number Size: ", chunk.NumberSize, " bytes")
		print(1, "Number Type: ", chunk.FloatingPoint and "Floating Point" or "Integer")
		print(0, "")
		print(0, "[Main]")
		printProto(indent, chunk.Main)
	end
end

return {Disassemble = Disassemble, Debug = Debug, Assemble = Assemble, ParseLASM = parseLASM};