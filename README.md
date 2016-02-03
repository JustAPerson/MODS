# MODS - Maximum Over Drive System
MODS is a toolkit to aid in the coding of Lua assembly.
MODS includes a custom linker and assembler.
Together they can easily be used to code in LASM.

In order to better understand the abilities of Lua assembly, read
[A No-Frills Introduction to Lua 5.1 VM Instructions][nofrills].

[nofrills]: http://luaforge.net/docman/83/98/ANoFrillsIntroToLua51VMInstructions.pdf

## Usage
MODS can be used as a library or a standalone application.


### Application
```
$ ./mods input.lasm -o output.luac
```

| flags | description |
|---|---|
| `-o` / `--output` | Change output file |
| `-s` / `--strip`  | Strips debugging information of existing bytecode |
| `-d` / `--disassemble` | Disassemble `.luac` bytecode file |


### Library
```lua
-- include the mods directory directly in the root source directory
local mods = require 'MODS.src.mods'

function test()
	print("Hello, world!")
end

local bytecode = string.dump(test)
print(mods.disassemble(bytecode))
```
```
.name "@MODS/test.lua"
.options 0, 0, 0, 2
GETGLOBAL 0, <"print">
LOADK 1, <"Hello, world!">
CALL 0, 2, 1
RETURN 0, 1
```

## Syntax
MODS employs a simple syntax that closely matches the structure of Lua
bytecode. Nearly every aspect of the emitted bytecode can be controlled.
Before learning the syntax utilized in MODS, however, it is important to
understand Lua assembly in general as explained by [this document][nofrills].

### Comments
Anything following a semicolon (`;`) is ignored.

```
; This is a comment
INSTRUCTION 0, 1, 2    ; this is also a constant
```

### Instructions
Instructions are identifiers followed by a comma-separated list of parameters.
These parameters may be labels, constants, or just numbers that control special
semantics.

```
.options 0, 0, 0, 3
GETGLOBAL 0, <"print">
LOADK 1, <"Hello, world!"> ; parameters inside arrow brackets are automatically declared constants
LOADK 2, <333/106> ; they can perform simple math!
CALL 0, 2, 1
RETURN 0, 1
```


## License
MODS is licensed under the terms of the MIT license. See `./LICENSE` for a full description thereof.

