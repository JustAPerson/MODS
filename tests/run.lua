package.path = package.path .. ";./src/?.lua;"
require "tests.current.disassemble"
require "tests.current.disassemble2"
require "tests.current.strip"