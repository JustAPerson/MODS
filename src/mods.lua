#!/usr/bin/env lua5.1

-- Lua only supports absolute paths for file inclusion
-- So append the search path to include our directories
package.path = package.path .. ";src/?.lua;MODS/src/?.lua"
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
	strip = false,
	disassemble = false,
}
local free = 0;

local i = 1;
while (i <= #args) do
	local arg = args[i]
	if arg == "-o" or arg == "--output" then
		local path = args[i + 1]
		if not path then
			error("Expected output file path after `-o` flag")
		end

		config.output = path
		i = i + 1 -- skip extra element
	elseif arg == "-s" or arg == "--strip" then
		config.strip = true
	elseif arg == "-d" or arg == "--disassemble" then
		config.disassemble = true
	else
		if config.input then
			-- already chosen input, unknown option
			error("Unexpected argument `" .. arg .. "`")
		end

		config.input = arg
	end
	i = i + 1
end

if not config.input then
	error("No input file specified")
end

local input_file = io.open(config.input, "r")
if not input_file then
	error("Unable to open input file: " .. config.input)
end

local input_contents = input_file:read("*all")
input_file:close()

local output_contents
if config.disassemble then
	if config.strip then
		error("Cannot strip debug information from disassembly")
	end

	output_contents = libmods.disassemble(input_contents)
else
	if config.strip then
		output_contents = libmods.strip(input_contents)

	else
		output_contents = libmods.assemble(input_contents)
	end

	-- these operations cannot reasonably be emitted to stdout
	if not config.output then
		-- no output specified, change file extension of input
		local path = config.input:match("(.+)%.")
		if not path then
			-- path has no file extension?
			path = config:match("(.+)")
		end
		config.output = path .. ".luac"
	end
end

-- output to file if specified, otherwise stdout
if config.output then
	local output_file = io.open(config.output, "w")
	if not output_file then
		error("Unable to open output file: " .. config.output)
	end
	output_file:write(output_contents)
	output_file:close()
else
	print(output_contents)
end
