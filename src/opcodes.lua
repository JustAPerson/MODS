return {
	[0] = "MOVE",
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

	-- opcode = {opcode_number, enum_type,  {parameter_types}, number_of_params}
	enum = {
		[0] = "ABC",
		"ABx",
		"AsBx",
	},
	list = {
		"A",
		"B",
		"C",
	},
	-- Parameter types (likely to change):
		-- Unused/Arbitrary = 0
		-- Register 		= 1
		-- Constant 		= 2
		-- Constant/Register= 3
		-- Upvalue 			= 4
		-- Jump Distace 	= 5

	MOVE			= {0, 0,	{1, 1, 5},	2};
	LOADK			= {1, 1,	{1, 2},		2};
	LOADBOOL		= {2, 0,	{1, 0, 0},	3};
	LOADNIL			= {3, 0,	{1, 1, 1},	3};
	GETUPVAL		= {4, 0,	{1, 4, 5},  2};
	GETGLOBAL		= {5, 1,	{1, 2},		2};
	GETTABLE		= {6, 0,	{1, 1, 3},	3};
	SETGLOBAL		= {7, 1,	{1, 2},		2};
	SETUPVAL		= {8, 0,	{1, 4, 5},	2};
	SETTABLE		= {9, 0,	{1, 3, 3},	3};
	NEWTABLE		= {10, 0,	{1, 0, 0},	3};
	SELF			= {11, 0,	{1, 1, 3},	3};
	ADD				= {12, 0,	{1, 1, 3},	3};
	SUB				= {13, 0,	{1, 1, 3},	3};
	MUL				= {14, 0,	{1, 1, 3},	3};
	DIV				= {15, 0,	{1, 1, 3},	3};
	MOD				= {16, 0,	{1, 1, 3},	3};
	POW				= {17, 0,	{1, 1, 3},	3};
	UNM				= {18, 0,	{1, 1, 5},	2};
	NOT				= {19, 0,	{1, 1, 5},	2};
	LEN				= {20, 0,	{1, 1, 5},	2};
	CONCAT			= {21, 0,	{1, 1, 1},	3};
	JMP				= {22, 2,	{0, 5},		1};
	EQ				= {23, 0,	{1, 3, 3},	3};
	LT				= {24, 0,	{1, 3, 3},	3};
	LE				= {25, 0,	{1, 3, 3}, 	3};
	TEST			= {26, 0,	{1, 5, 1},	2};
	TESTSET			= {27, 0,	{1, 1, 1},	3};
	CALL			= {28, 0,	{1, 0, 0},	3};
	TAILCALL		= {29, 0,	{1, 0, 0},	3};
	RETURN			= {30, 0,	{1, 0, 5},	2};
	FORLOOP			= {31, 2,	{1, 0},		2};
	FORPREP			= {32, 2,	{1, 0},		2};
	TFORLOOP		= {33, 0,	{1, 5, 0},	2};
	SETLIST			= {34, 0,	{1, 0, 0},	3};
	CLOSE			= {35, 0,	{1, 5, 5},	1};
	CLOSURE			= {36, 1,	{1, 0},		2};
	VARARG			= {37, 0,	{1, 1, 5},	2};
}