#!/usr/bin/env lua

--[[
Extracts luahelper.lua from Wacraft3.exe and prints it to STDOUT.

USAGE:
	lua "extract-luahelper.lua" <wc3 path> [replace debug]

EXAMPLE:
	lua "extract-luahelper.lua" "/path/to/warcraft3.exe" --no-debug > output-luahelper.lua

OPTIONS:
	--yes-debug to replace debug placeholder with true
	--no-debug  ... with false (Recommended)
]]

PATTERN_NONPRINTABLE = "([^\t\r\n\v -~])"
TEXT_ANCHOR = "-- Jass2 array"

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
	
	local from, to = buffer:reverse():find("A")
	
	assert(to, "Could not find a non-printable character searching backwards. Aborting.")
	
	local offsetStart = anchorOffset - to + 1
	return offsetStart
end

function findEndOffset(file, anchorOffset)
	local offsetEnd = anchorOffset
	
	file:seek("set", anchorOffset)
	for i = 1, 1000 do
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
	
	error("Could not find end of text searching for the end. Aborting")
end

function readContents(file, offset, length)
	file:seek("set", offset)
	return file:read(length)
end

function main()
	local exePath = arg[1]
	assert(exePath, "First argument must be a path to a Lua-capable 'Warcraft III.exe'!")
	
	local doReplaceDebug = false
	local replaceDebugText = "false"
	if arg[2] then
		if arg[2] == "--yes-debug" then
			doReplaceDebug = true
			replaceDebugText = "true"
		elseif arg[2] == "--no-debug" then
			doReplaceDebug = true
			replaceDebugText = "false"
		else
			error("Second argument must be either '--yes-debug' or '--no-debug'")
		end
	end
			

	local exe = assert(io.open(exePath, "rb"))
	io.stderr:write(string.format("exePath: %s\n", exePath))

	io.stderr:write(string.format("Searching for text anchor: '%s'\n", TEXT_ANCHOR))
	local anchorOffset = findAnchor(exe, TEXT_ANCHOR)
	io.stderr:write(string.format("anchorOffset: %d\n", anchorOffset))

	local offsetStart = findStartOffset(exe, anchorOffset)
	io.stderr:write(string.format("offsetStart: %d\n", offsetStart))
	local offsetEnd = findEndOffset(exe, anchorOffset)
	io.stderr:write(string.format("offsetEnd: %d\n", offsetEnd))
	local fileSize = offsetEnd - offsetStart
	io.stderr:write(string.format("luahelper.lua file size: %d\n", fileSize))
	
	local luahelperText = readContents(exe, offsetStart, fileSize)
	if doReplaceDebug then
		luahelperText = luahelperText:gsub("%$debug%$", replaceDebugText)
	end
	
	io.stdout:write(luahelperText)
	exe:close()
	io.stderr:write("bye-bye!\n")
end
main(arg)
