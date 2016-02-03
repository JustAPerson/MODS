# MODS - Maximum Over Drive System
MODS is a toolkit to aid in the coding of Lua Assembly.
MODS includes a custom linker and assembler. Together they can easily be used to code in LASM.

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

## License
MODS is licensed under the terms of the MIT license. See `./LICENSE` for a full description thereof.

