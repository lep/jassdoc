# Extra Lua

This folder contains additional files only relevant for the Lua API in Warcraft 3 Reforged and pre-Reforged, when Lua was first introduced.

- luahelper.lua: provides compatibility functions for the Lua runtime. It's always available and loaded before your scripts start.

## How to extract

### Automatically
	
1. Set file path: `export w3exe="$(cygpath 'D:\SteamLibrary\Warcraft III\_retail_\x86_64\Warcraft III.exe')"`
2. Run extract-luahelper.lua from tools folder: `lua "extract-luahelper.lua" --luahelper "$w3exe" --no-debug > output-luahelper.lua`
    - needs any `lua` interpreter installed

### Manually

0. Set file path: `export w3exe="$(cygpath 'D:\SteamLibrary\Warcraft III\_retail_\x86_64\Warcraft III.exe')"`
   - This is for the Cygwin Bash shell on Windows, `C:` paths only work there.
1. `strings --data -n 8 -t d "$w3exe" | grep -C2 --color -i __jarray`
   - this command is from the `binutils` package
2. Take the first offset +- and see what's there: `tail --bytes +12345 "$w3exe" | strings -n 1 | head -n 80`
	- 2.0.2.22796: starts at decimal offset 38097696
3. If that's what you want, output the text to a ` > file.txt` and edit it in Notepad++
