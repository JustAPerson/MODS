--[=[

			Maximum OverDrive System v1.0.0-beta
								MODS

		# WARNING: MODS v1 is now deprecated. It is legacy software now, do not expect updates for it #

~
~	Update:
~		- Fixes bug in Assemble() when handling NUL strings
~       - Addes a second parameter to Debug(), `bool Parsable`
~       	- If true, the output will be parsable by parseLASM()
~

What is MODS v1 ?
MODS v1 is the combination of MODS v0.5 and LuaDbg.
They are now formally being mixed into one project.

Now, I do not suggest the usage of MODS v1, because MODS v2 is under development
and almost ready. v2 is currently not functional, but it will be soon.

		###########################################
		#														#
		#		Here is the MODS v0.5 Docs				#
		#														#
		###########################################

		Maximum Overdrive System
			CODENAME: MODS

Chapters:
1.) Intro
2.) Syntax
	2.0) Basic code parsing
	2.1) Opcode usage
	2.2) Control Code usage
		2.2.1) Changing Chunk properties
	2.3) Declaring Constants, Locals, and Upvalues
	2.4) Declaring functions
		2.4.1) Using Upvalues in functions
		2.4.2) Using Varargs in functions
3.) Changelog




= = = = CHAPTER 1 = = = =

	This is the formal documentation of the MODS Lua Assembly suite.

	In the following chapters you will learn:
		The history of the MODS system
		How to properly use the MODS Syntax
		Maybe a few random Lua tricks

	WHAT YOU WILL NOT LEARN IN THE FOLLOWING CHAPTERS:
		To read / comprehend Lua Assembly
	If you are looking to learn Lua Assembly, go get the "ANoFrillsIntroToLua51VMInstructions.pdf"
	file from LuaForge.


	CURRENTLY, AS OF VERSION v0.5, THERE IS:
		FULL SUPPORT FOR MULTIPLE DATATYPES
		FULL COMMENT SUPPORT (; denotes a comment)
		SUPPORT FOR TABBING AND BLANK LINES AND LINES STARTING WITH COMMENTS
		NO SUPPORT FOR MULTI-LINE STRINGS
		NO SUPPORT FOR MULTIPLE INSTRUCTIONS / LINE

	### MODS IS DESIGNED FOR NORMAL LUA ONLY, INCOMPATIBLE WITH RBX.LUA ###
	## MEANING, YOU CANNOT RUN MODS IN ROBLOX.EXE, ONLY LUA.EXE OR SOMETHING SIMILAR ##


	~!~ DON'T READ THE SOURCE CODE, IT'S PRETTY NASTY. IT WERKS, BUT JUST BARELY. SOMETIMES ERRORS ARE REALLY INDECIPHERABLE (espicially if it's a problem with running the code). ~!~

= = = = CHAPTER 2 = = = =



= = SECTION 0 = =


To parse a block of LASM code, use something along the following.

local bytecode = parseLASM([[
; put LASM code here
]])


string parseLASM(string LASM)
the parseLASM will return the bytecode string of the assembled code, equiveleant to string.dump()
You can then run your code like this:

print(pcall(loadstring(bytecode)))


= = SECTION 1 = =


Each LASM opcode can be denoted by starting a new line with it's name (case insensitive).
The opcode's parameters are seperated by spaces.

Example:
move 1 0
Move 2 1
MOVE 3 2

When supplying a parameter, any and all constants should be prepended by the letter "k"

Example:
getglobal 0 k0


= = = SECTION 2 = = =


There are currently 7 control codes (Ctrl codes), each one serving a different purpose.
ctrl codes are used to define properties of chunks, and declare values.
All ctrl codes are denoted by prepending a period (".") infront of their name.

There is:
	.const	- Declares a new constant value in the current chunk
	.local 	- Declares a new local value in the current chunk
	.upval 	- Declares a new upvalue in the current chunk
	.optns	- Changes the current chunk's properties
	.cname	- Changes the current chunk's source name
	.funct	- starts a new chunk
	.end	- Ends a chunk


= = SUB-SECTION 1 = =

You can change 5 chunk properties with the ctrl codes .optns and .cname

.optns has 4 arguments, all of which change 1 property of a chunk.
The arguments, in order are, Number of Upvalues, Number of Arguments,
Variable Argument Flag, and Maximum Stack Size.

.cname has only 1 argument, which is what the sourcename of a chunk should be


Example:
.optns 5 6 3 9
.cname "AChunk"

A segment of the preceding chunk's disassembly:
Source Name "AChunk", Lines 1 to 1
Upvalues : 5, Arguments : 6, VargFlag : 3, Max Stack Size : 9

= = = SECTION 3 = = =


You can declare a constant, local value, or upvalue, using each of their own respective
ctrl codes.

Example:
.const 3.14
.local 'text'
.upval 'text'


= = = SECTION 4 = = =


You can declare a new function prototype using the .funct ctrl code
you then can denote the end of that prototype with the .end ctrl code
(the embedding of mutliple prototypes is supported)

Example:
Normal Lua:

local function coolprint() print("MAGIC!") end
coolprint()
>MAGIC!

LASM:

.local "coolprint"
.funct
	.const "print"
	.const "MAGIC!"
	getglobal 0 k0
	loadk 1 k1
	call 0 2 1
.end
closure 0 0
call 0 1 1

>MAGIC!

NOTE:
the functionality of the .optns ctrl code is also included with the .funct ctrl code.
So you can add four extra arguments to .funct (not required though)


= = SUB-SECTION 1 = =

Example usage of Upvalues
Lua Version:

local text = "Woah!"
local function fucn() print(text) end
func()
>Woah!

LASM Version:

.local "text"
.local "func"
.const "Woah!"
.funct 1 0 0 2
	.upval "text"
	.const "print"
	getglobal 0 k0
	getupval 1 0
	call 0 2 1
.end
loadk 0 k0
closure 1 0
move 0 0
call 1 1 1
>Woah!

= = SUB-SECTION 2 = =

Example Usage of Variable Arguments:
Lua verison:

local function coolprint(...) print(...) end
coolprint("abc", 3, "why", 0)
>abc	3	why	0

LASM Version:

.local "coolprint"
.const "abc"
.const 3
.const "why"
.const 0
.funct 0 0 3 3
	.const "print"
	getglobal 0 k0
	vararg 1 0
	call 0 0 1
.end
closure 0 0
loadk 1 k0
loadk 2 k1
loadk 3 k2
loadk 4 k3
call 0 5 1
>abc	3	why	0

(Interesting Note: whenever you use a vararg, if you declare the local variable "arg", it will become a table filled with all of the arguments)



= = = = CHAPTER 3 = = = =



	CHANGELOG
		v0.5
		 - Actually fixed the encoding of sBx opcodes
		   - Turns out I did it wrong before (because I never tested)
       - Lots more comment support (so put them everywhere!)
		v0.4
		 - Improved documentation
		 - Fixed a problem with the JUMP opcode's assembler
		 - Fixed a problem with the encoding of sBx opcodes

		v0.3
		 - Added support for chunk customization
		 - Added support for Local values, Upvalues, Function values, and Variable Arguments
		 - Completely rewrote the Parser / Assembler interface

		V0.2
		 - Completely rewrote the Parser (multiple times)

		v0.1
		 - Improved on the parser in several places
		 - Attempted support for local values, upvalues, function values with the help of Linoleum
		    - Never finished

		v0.0
		 - Concieved the idea of how the parser currently works.
		 - Wrote a very primitive version of the current parser.


Thanks for reading!


		###########################################
		#														#
		#			Here is the LuaDbg Docs				#
		#														#
		###########################################
]=]
--
--		Thank you for choosing LuaDbg!
--			By: 					NecroBumpist
--			Latest Revision: 	v1.01 (27/5/2011)
--
--		So, now a quick description of the LuaDbg library.
--		LuaDbg aims to provide a comprehensive set of tools to work with Lua Bytecode,
--		regardless of whether or not you wish to work with the raw Bytecode, or Lua Assembly.
-- 	Now you might be asking, `What the heck is Lua Assembly or Bytecode,` so I'll tell you.
--
--		Many believe that Lua is interpreted, but it is infact a compiled language.
--		But the compilation of a Lua script is done usually when the script is first ran.
--		Lua does not compile to x86 assembly, but instead a special language aptly called
--		Lua assembly. Lua Assembly, or LASM, is ran by the LuaVM, and that's where the magic happens.
--
--		I'll spare most of you the boring discussion of LASM, but incase you want to learn it,
--		I suggest you read the 'ANoFrillsIntroToLua51VMInstructions.pdf' document, aviable on the
--		LuaForge website.
--
--		In this short README file, I will only cover the basic usage of the Rip(), and Debug() functions.
-- 	I will document more of the Assemble() and Disassemble() functions at a later time.

-- 			## SPECIAL NOTE: 	RobloxApp.exe does not properly display tabs, so the Debug()
--				## 					function does not work entirely as inteded.

-- Rip()
repeat
	wait();					-- Wait for the library.
until _G.lasm
local lasm = _G.lasm;

local function test()	-- Create an example function prototype.
	local d = "4";

	return (function()
		local a, b, c = "1", "2", "3";
		return a .. b .. c .. d;
	end)()
end

local a = string.dump(test);		-- Dump the example function into bytecode.
local b = lasm.Rip(a);				--	Remove the bytecode's debug information.

print("Function length with debug info: " .. #a);
print("Function length without debug info: " .. #b);

print(loadstring(a)())				-- Confirm both functions work properly
print(loadstring(b)())				-- Should print 'abcd'


-- Debug()
repeat
	wait();					-- Wait for the library.
until _G.lasm
local lasm = _G.lasm;

local function test()	-- Create an example function prototype.
	local d = "4";

	return (function()
		local a, b, c = "1", "2", "3";
		return a .. b .. c .. d;
	end)()
end

local a = string.dump(test);		-- Dump the example function into bytecode.
lasm.Debug(a);							-- Disassemble the function, printing everything out;

-- Proper Output (I think, tabbed this out by hand)
--[[
LuaDbg.lua
[Chunk Header]
	Identifier: Lua
	Version: 0x51
	Format: Official
	Endianness: Litte
	Integer Size: 4 bytes
	String Size Size: 4 bytes
	Instruction Size: 4 bytes
	Number Size: 8 bytes
	Number Type: Floating Point
	[Main]
		Name: "=Workspace.LuaDbg.README" Lines: 55-62
		Upvalues: 0 Arguments: 0 VargFlag: 0 MaxStackSize: 2
		[Instructions] Count: 6
			[1] (56) Opcode: LOADK 0 0 ; ![d] ('4')
			[2] (61) Opcode: CLOSURE 1 0 ; 1 0
			[3] (61) Opcode: MOVE 0 0 0 ; [d] [d] 0
			[4] (61) Opcode: TAILCALL 1 1 0 ; 1 1 0
			[5] (61) Opcode: RETURN 1 0 0 ; 1 0 0
			[6] (62) Opcode: RETURN 0 1 0 ; 0 1 0
		[Constants] Count: 1
			[0] Type: (String) "4"
		[Locals] Count: 1
			[0] SPC: 1 EPC: 5 Name: "d"
		[Upvalues] Count:0
		[Prototypes] Count: 1
		[0]
			Name: "" Lines: 58-61
			Upvalues: 1 Arguments: 0 VargFlag: 0 MaxStackSize: 7
			[Instructions] Count: 10
				[1] (59) Opcode: LOADK 0 0 ; 0 ('1')
				[2] (59) Opcode: LOADK 1 1 ; 1 ('2')
				[3] (59) Opcode: LOADK 2 2 ; ![c] ('3')
				[4] (60) Opcode: MOVE 3 0 0 ; 3 [a] 0
				[5] (60) Opcode: MOVE 4 1 0 ; 4 [b] 0
				[6] (60) Opcode: MOVE 5 2 0 ; 5 [c] 0
				[7] (60) Opcode: GETUPVAL 6 0 0 ; 6 <d> 0
				[8] (60) Opcode: CONCAT 3 3 6 ; 3 3 6
				[9] (60) Opcode: RETURN 3 2 0 ; 3 2 0
				[10] (61) Opcode: RETURN 0 1 0 ; 0 1 0
			[Constants] Count: 3
				[0] Type: (String) "1"
				[1] Type: (String) "2"
				[2] Type: (String) "3"
			[Locals] Count: 3
				[0] SPC: 3 EPC: 9 Name: "a"
				[1] SPC: 3 EPC: 9 Name: "b"
				[2] SPC: 3 EPC: 9 Name: "c"
			[Upvalues] Count:1
				[0] Name: "d"
			[Prototypes] Count: 0
]]



--
--				QUICK OVERVIEW OF Debug() OUTPUT;
--
--		Steps:
--			- Print out the information contained in the 12 byte header
--			- Begin debuging of the main function prototype
--				- Print the Function name, lines, and several VM specifications
--				- Print out the instructions
--					- [Opcode Number] (Line Number)	Opcode: <opcode name> <parameters>; parameter description
--						- ('') 	= constants
--						- <>		= upvalues
--						- [] 		= predefined local register
--						- ![] 	= newly defined local register
--				- Print out the constants
--				- Print out locals
--				- Print out upvalues
--				- Recursively debug prototypes
--
