require "busted"
local dis = require "src.current.disassemble"

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