--[[

			Maximum OverDrive System v2.2.0-pre-alpha
								MODS

	I've yet to write the documentation, I'll do so when I finish the assembler.
	Though I will keep a change log here.

Change Log:

	Alpha Stage:
		v2.2.0:
			- General work on the assembler
			- Finished Macros
			- Added a ".const" directive finally.
		v2.1.0:
			- Finished Identifiers
			- Finished most of the basic identifiers
			- Finished instruction and expression parsing
			- Removed Lua based assembler macros in favor of NASM style macros.
			- Assembler now in a working state.
	Pre-Alpha Stage:
		v2.0.1:
			- More work on Identifiers
			- Stared directives, yet to fully implement .local
			- Laying the base for Instructions so we can finally finish
			- Finished labels I think
			- Oh yeah, Macros are cool
		v2.0.0:
			- Finally realesed MODS in my place so it can be right next to my bio
			- renaming the old Assemble and Disassemble functions to Link and Delink respectively
			- Stole some MORE of xLEGOx's wonderful code, this time it is the Lexer/Tokenizer thing that is used extensively in the Assembler

]]