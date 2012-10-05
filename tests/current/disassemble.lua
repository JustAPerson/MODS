require "busted"
local dis = require "src.current.disassemble"

describe("dis_param", function()
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
		{Opcode = "LOADK", A = 0, B = 1},
		{Opcode = "LOADK", A = 0, B = 2},
		{Opcode = "LOADK", A = 0, B = 3},
	}
	it("constant nil", function()
		assert.is.equal(dis.dis_param(proto, instrs[1], 2), "<nil>")
	end)
	it("constant bool", function()
		assert.is.equal(dis.dis_param(proto, instrs[2], 2), "<true>")
	end)
	it("constant number", function()
		assert.is.equal(dis.dis_param(proto, instrs[3], 2), "<3.1415>")
	end)
end)