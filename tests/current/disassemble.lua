require "busted"
local dis  = require "src.current.disassemble"
local mods = require "src.current"

local proto = {
	Constants = {
		[0] = {
			ConstantType = "nil";
			Value = nil;
		},
		[1] = {
			ConstantType = "bool";
			Value = true;
		},
		[2] = {
			ConstantType = "number";
			Value = 3.1415;
		},
		[3] = {
			ConstantType = "string";
			Value = "test tset";
		},
	};
}

local instrs = {
	{Opcode = "LOADK", A = 0, B = 0},
	{Opcode = "LOADK", A = 1, B = 1},
	{Opcode = "LOADK", A = 2, B = 2},
	{Opcode = "LOADK", A = 3, B = 3},
	{Opcode = "MOVE",  A = 1, B = 5},
}

describe("dis_param", function()
	it("constant nil", function()
		assert.is.equal(dis.dis_param(proto, instrs[1], 2), "<nil>")
	end)
	it("constant bool", function()
		assert.is.equal(dis.dis_param(proto, instrs[2], 2), "<true>")
	end)
	it("constant number", function()
		assert.is.equal(dis.dis_param(proto, instrs[3], 2), "<3.1415>")
	end)

	it("constant string", function()
		assert.is.equal(dis.dis_param(proto, instrs[4], 2), "<\"test tset\">")
	end)
end)

describe("dis_instr", function()
	it("", function()
		assert.is.equal(dis.dis_instr(proto, instrs[1]), "LOADK 0, <nil>")
	end)
	it("", function()
		assert.is.equal(dis.dis_instr(proto, instrs[2]), "LOADK 1, <true>")
	end)
	it("", function()
		assert.is.equal(dis.dis_instr(proto, instrs[3]), "LOADK 2, <3.1415>")
	end)
	it("", function()
		assert.is.equal(dis.dis_instr(proto, instrs[4]), "LOADK 3, <\"test tset\">")
	end)


	it("", function()
		assert.is.equal(dis.dis_instr(proto, instrs[5], "MOVE 1, 5"))
	end)
end)

local function test(func)
	local old = string.dump(func)
	local dis = mods.disassemble(old)
	local co = coroutine.create(function() return loadstring(mods.assemble(dis)) end)
	local pass, new, r = coroutine.resume(co)

	if not (pass and new) then
		print(dis)
		print(debug.traceback(co))
		print("Error: ", r)
	end

	local a, b = new(), func()
	assert.is.equal(a, b)
end

describe("disassemble", function()
	it("disassemble test 1", function()
		test(function()
			local function add(a, b)
				return a+b
			end
			return add(add(1,6), add (2,9))
		end)
	end)

	it("disassemble test 2", function()
		test(function()
			local a = 0
			for i = 1, 100 do
				a = a + i + i*a
			end
			return a
		end)
	end)

	it("disassemble test 3", function()
		test(function()
			local x = 5
			local function change()
				x = x + 2
			end
			change()
			return x
		end)
	end)
end)