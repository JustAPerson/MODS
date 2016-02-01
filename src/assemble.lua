local OpSpecs = require "opcodes"
local Link = require "link"

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

			spc, epc = n1.data, n2.data;
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
		local name = stream.expectString().data;

		state.Current.Name = name;
		--[[state.Current:PushIdent{
			type = "proto";
			name = name;
			value = state.Current.num;
		};]]
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

return {
	assemble = Assemble;
}
