#!/usr/bin/env lua5.1

-- Lua only supports absolute paths for file inclusion
-- So append the search path to include our directories
package.path = package.path .. ";src/?.lua;mods/src/?.lua"
local libmods = {
	assemble    = require "assemble".assemble;
	disassemble = require "disassemble".disassemble;
	strip       = require "strip";
	link        = require "link";
	delink      = require "delink";
}

if pcall(getfenv, 4) then
	-- crude approximation of python's `__name__ == "__main__"`
	-- included as a library
	return libmods
end

local args = { ... }
if #args == 0 then
	error("Arguments expected")
end

local config = {
	input = nil,
	output = nil,
}
local free = 0;

local i = 1;
while (i <= #args) do
	local arg = args[i]
	if arg == "-o" then
		local path = args[i + 1]
		if not path then
			error("Expected output file path after `-o` flag")
		end

		config.output = path
		i = i + 2
	else
		if config.input then
			-- already chosen input, unknown option
			error("Unexpected argument `" .. arg .. "`")
		end

		config.input = arg
		i = i + 1
	end
end

if not config.input then
	error("No input file specified")
end
if not config.output then
	-- no output specified, change file extension of input
	local path = config.input:match("(.+)%.")
	if not path then
		-- path has no file extension?
		path = config:match("(.+)")
	end

	config.output = path .. ".luac"
end

local input_file = io.open(config.input)
if not input_file then
	error("Unable to open input file: " .. config.input)
end
local output_file = io.open(config.output, "w")
if not output_file then
	error("Unable to open output file: " .. config.output)
end

local input_contents = input_file:read("*all")
local bytecode = libmods.assemble(input_contents)

output_file:write(bytecode)

input_file:close()
output_file:close()
