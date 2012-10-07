local delink = require "current.delink"
local link   = require "current.link"

local function strip_proto(proto)
	proto.Name = ""

	for i = 0, proto.Instructions.Count-1 do
		proto.Instructions[i].LineNumber = nil
	end

	proto.Locals.Count = 0
	proto.Upvalues.Count = 0

	for i = 0, proto.Protos.Count-1 do
		strip_proto(proto.Protos[i])
	end
end

return function(input)
	if type(input) == "function" then
		input = string.dump(input)
	elseif not (type(input) == "string" and input:sub(1,1) == "\27") then
		error("Expected function or bytecode", 2)
	end
	local proto = delink(input)
	strip_proto(proto.Main)
	return link(proto)
end