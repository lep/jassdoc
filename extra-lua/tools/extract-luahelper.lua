#!/usr/bin/env lua

USAGE = [[
Extracts luahelper.lua from Wacraft3.exe and prints it to STDOUT.

USAGE:
	lua "extract-luahelper.lua" <which file option> <wc3/memory dump path> [replace debug]

EXAMPLE:
	lua "extract-luahelper.lua" --luahelper "/path/to/warcraft3.exe" --no-debug > output-luahelper.lua
	lua "extract-luahelper.lua" --commonj "/path/to/warcraft3-memory.dump" > output-luahelper.lua

OPTIONS:
	Which file:
	--luahelper to extract luahelper.lua from EXE
	--commonj   to extract common.j.lua from memory dump
	--blizzardj   to extract blizzard.j.lua from memory dump
	--commonai   to extract common.ai.lua from memory dump

	Replace debug in luahelper:
	--yes-debug to replace debug placeholder with true
	--no-debug  ... with false (Recommended)
	            default: do not replace anything

COMMENT:
	initcheats.j and cheats.j don't exist, because I couldn't get them to load
	or find in the memory dump.
	
AUTHOR:
	Luashine
]]


PATTERN_NONPRINTABLE = "([^\t\r\n\v -~])"
MODE = nil -- alias object for the selected MODES Subtable
MODES = {
	LUAHELPER = {
		SOURCE_PRETTY_NAME = "EXE file",
		TEXT_ANCHOR = "-- Jass2 array",
		OUTPUT_NAME = "luahelper.lua"
	},
	COMMONJ = {
		SOURCE_PRETTY_NAME = "Memory dump",
		TEXT_ANCHOR = "TypeDefine('agent', 'handle')",
		OUTPUT_NAME = "common.j.lua"
	},
	BLIZZARDJ = {
		SOURCE_PRETTY_NAME = "Memory dump",
		TEXT_ANCHOR = "bj_PI = 3.14159",
		OUTPUT_NAME = "blizzard.j.lua"
	},
	COMMONAI = {
		SOURCE_PRETTY_NAME = "Memory dump",
		TEXT_ANCHOR = [[UPG_SORCERY = FourCC("Rhst")]],
		OUTPUT_NAME = "common.ai.lua"
	},
}

function getSourcePrettyName()
	return MODE.SOURCE_PRETTY_NAME
end

function getTextAnchor()
	return MODE.TEXT_ANCHOR
end

function getOutputName()
	return MODE.OUTPUT_NAME
end

function findAnchor(file, text)
	local offset = 0
	local bufferSize = 64 * 1024
	local step = bufferSize - #text - 1
	local anchorOffset = nil
	
	repeat
		local ok, err = file:seek("set", offset)
		if not ok then
			return false, err
		end
		
		local buffer = file:read(bufferSize)
		local from, to = buffer:find(text, 1, true)
		
		if from then
			anchorOffset = offset + from - 1
		end
		offset = offset + step
	until anchorOffset
	
	return anchorOffset
end

function findStartOffset(file, anchorOffset)
	local bufferSize = 4096
	file:seek("set", anchorOffset - bufferSize)
	local buffer = file:read(bufferSize)
	
	local from, to = buffer:reverse():find(PATTERN_NONPRINTABLE)
	
	assert(to, "Could not find a non-printable character searching backwards within ".. tostring(bufferSize) .." bytes. Aborting.")
	
	local offsetStart = anchorOffset - to + 1
	return offsetStart
end

function findEndOffset(file, anchorOffset)
	local lineLimit = 30 * 1000

	local offsetEnd = anchorOffset
	
	file:seek("set", anchorOffset)
	for i = 1, lineLimit do
		local line = file:read("*l")
		local from, to, match = line:find(PATTERN_NONPRINTABLE)
		if to then
			offsetEnd = offsetEnd + to - 1
			return offsetEnd
		else
			-- "*l" pattern swallowed line characters.
			-- Account for \n only
			offsetEnd = offsetEnd + #line + 1
		end
	end
	
	error("Could not find end of text searching for the end. Line limit=".. tostring(lineLimit) ..". Aborting")
end

function readContents(file, offset, length)
	file:seek("set", offset)
	return file:read(length)
end

function main()
	if #arg == 0 or arg[1] == "-h" or arg[1] == "--help" then
		print(USAGE)
		os.exit(0)
	end
	
	local targetMode = arg[1]
	assert(targetMode:sub(1,2) == "--", "First argument must specify which file to extract!")
	targetMode = targetMode:sub(3):upper()
	if MODES[targetMode] then
		MODE = MODES[targetMode]
	else
		local availableModes = {}
		local modesText
		for name, tbl in pairs(MODES) do
			table.insert(availableModes, "--".. name)
		end
		modesText = table.concat(availableModes, "\n")
		io.stderr:write([[
The first argument '%s' is not a valid mode. See --help.
It must be one of:
%s
]],		targetMode, modesText)
	end

	local sourcePath = arg[2]
	assert(sourcePath, "Second argument must be a path to a Lua-capable 'Warcraft III.exe' or a memory dump!")
	
	local doReplaceDebug = false
	local replaceDebugText = "false"
	if arg[3] then
		if arg[3] == "--yes-debug" then
			doReplaceDebug = true
			replaceDebugText = "true"
		elseif arg[3] == "--no-debug" then
			doReplaceDebug = true
			replaceDebugText = "false"
		else
			error("Second argument must be either '--yes-debug' or '--no-debug'")
		end
	end
			

	local exe = assert(io.open(sourcePath, "rb"))
	io.stderr:write(string.format("%s Path: '%s'\n", getSourcePrettyName(), sourcePath))

	io.stderr:write(string.format("Searching for text anchor: '%s'\n", getTextAnchor()))
	local anchorOffset = findAnchor(exe, getTextAnchor())
	io.stderr:write(string.format("anchorOffset: %d\n", anchorOffset))

	local offsetStart = findStartOffset(exe, anchorOffset)
	io.stderr:write(string.format("offsetStart: %d\n", offsetStart))
	local offsetEnd = findEndOffset(exe, anchorOffset)
	io.stderr:write(string.format("offsetEnd: %d\n", offsetEnd))
	local fileSize = offsetEnd - offsetStart
	io.stderr:write(string.format("%s file size: %d\n", getOutputName(), fileSize))
	
	local outputText = readContents(exe, offsetStart, fileSize)
	if doReplaceDebug then
		outputText = outputText:gsub("%$debug%$", replaceDebugText)
	end
	
	io.stdout:write(outputText)
	exe:close()
	io.stderr:write("bye-bye!\n")
end
main(arg)
