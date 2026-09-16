# Extra Lua

This folder contains additional files only relevant for the Lua API in Warcraft 3 Reforged and pre-Reforged, when Lua was first introduced.

- luahelper.lua: provides compatibility functions for the Lua runtime. It's always available and loaded before your scripts start.
- builtin.lua: functions internally defined by the game

## How to extract luahelper

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

## How to extract transpiled Jass files

1. `common.j` and `Blizzard.j` are always included.
For `common.ai`, you must start a Lua map with AI players active.
2. Once loaded, use a tool like Process Explorer to create a full memory dump of `Warcraft III.exe`
3. Run `extract-luahelper.lua` with respective arguments, see `--help`

**Note:** native definitions are not transpiled to Lua, they are
silently omitted (works as intended).

Bash:

```bash
function extractDump() {
	local dumpPath="$1";
	if [[ ! -e "$dumpPath" ]]; then
		echo "dump file '$dumpPath' doesn't exist!" >&2
		exit 1
	fi
	for jass in "common.j" "blizzard.j" "common.ai"; do
		lua extract-luahelper.lua "--$j{jass//./}" "$dumpPath" \
			> "$dumpPath-$jass.lua"
	done
}
# run with: extractDump /path/to/war3.dmp
```

I will try to keep the dumped files updated here, they are unrelated to Jassdoc:
[Luashine/jass-history: /lua-dump](https://github.com/Luashine/jass-history/tree/master/lua-dump)
