
/**
Applies the specified cheat, but only if the game is single player. There are a
few cheats that can be toggled on or off. If the cheat is enabled, entering the
cheat again will disable it. If the cheat is disabled, entering the cheat will
enable it again. Upon entering, the text "Cheat Enabled!" will be displayed.

@param cheatStr
The cheat to enter. 

@note For a list of all cheats see <http://classic.battle.net/war3/cheatcodes.shtml>.
*/
native Cheat            takes string cheatStr returns nothing

native IsNoVictoryCheat takes nothing returns boolean

native IsNoDefeatCheat  takes nothing returns boolean



/**
It does two things:
1) Try to read the file, if "Allow Local Files" is enabled then also searches in the game folder
2) Append filename to preload buffer

@param filename Text string, supposed to be a file path to be preloaded. Max length: 259 characters (see Windows MAX_PATH).

@note The game only reads these files, does not load them. The reading is done in a separate thread and does not freeze the game. One file is not read twice, no matter how often you call Preload().

@note Trick: It does not escape double-quotes " on purpose (approved not a bug, it's a feature).
It is possible to inject custom code in Preload files this way (Lua):

    PreloadGenClear()
    PreloadGenStart()
    Preload(' ")\ncall otherFunction("123")\n//')
    PreloadGenEnd("its-a-feature.txt")
	
Results in the following preload file code (Jass):

    function PreloadFiles takes nothing returns nothing
     
            call PreloadStart()
            call Preload( " ")
    call otherFunction("123")
    //" )
            call PreloadEnd( 754.6 )
     
    endfunction


@note **Game folder:**
Reforged: `Warcraft III\_retail_\somefile.txt`, instead of `_retail_` there's also a `_ptr_` game version currently.
Classic: ?

@note **Mini tutorial:**

**What are Preload files?**

Preload files instruct the game to pre-read a file/resources to avoid freezes/stutter during gameplay. It's done to move the file into OS cache. Blizzard used preload files to load all required files at map init. See blizzard.j or campaign maps.

Create a preload file (Lua)

    PreloadGenClear()
    PreloadGenStart()
    -- call Preload("filename.ext") as often as you need, one call per file you add
    Preload("Textures\\Knight.blp")
    PreloadGenEnd("MyPreloadFile.txt")

**How to run a preload file**

This must be done manually:

	Preloader("MyPreloadFile.txt")
	
**Lua code in preload files?**

It is possible although in a very hacky way, [described here](https://www.hiveworkshop.com/threads/blizzards-hidden-jass2lua-transpiler.337281/).
You need to use "//! beginusercode" to start a section containing Lua code and end it using "//! endusercode".
It works because the code is transpiled on the fly with Jass2Lua.


@note See: `PreloadEnd`, `PreloadStart`, `PreloadRefresh`, `PreloadEndEx`, `PreloadGenClear`, `PreloadGenStart`, `PreloadGenEnd`, `Preloader`
@note Also see the documentation for `Preloader` for more info on the generated files.
*/
native Preload          takes string filename returns nothing

/**
Unknown. It's always generated at the end of a preload file, timeout represents the time between calls to `PreloadStart` and `PreloadGenEnd`.

@note See: `Preload`, `PreloadStart`, `PreloadRefresh`, `PreloadEndEx`, `PreloadGenClear`, `PreloadGenStart`, `PreloadGenEnd`, `Preloader`
*/
native PreloadEnd       takes real timeout returns nothing


/**
Clears the preload buffer and starts the timer. (Anything else?)

@note See: `Preload`, `PreloadEnd`, `PreloadRefresh`, `PreloadEndEx`, `PreloadGenClear`, `PreloadGenStart`, `PreloadGenEnd`, `Preloader`
*/
native PreloadStart     takes nothing returns nothing

/**
Unknown. It does not reset the timer or clear the buffer.

@note See: `Preload`, `PreloadEnd`, `PreloadStart`, `PreloadEndEx`, `PreloadGenClear`, `PreloadGenStart`, `PreloadGenEnd`, `Preloader`
*/
native PreloadRefresh   takes nothing returns nothing

/**
Unknown

@note See: `Preload`, `PreloadEnd`, `PreloadStart`, `PreloadRefresh`, `PreloadGenClear`, `PreloadGenStart`, `PreloadGenEnd`, `Preloader`
*/
native PreloadEndEx     takes nothing returns nothing



/**
Clears all added file paths from the current preload buffer. Does not reset the timer.

@note See: `Preload`, `PreloadEnd`, `PreloadStart`, `PreloadRefresh`, `PreloadEndEx`, `PreloadGenStart`, `PreloadGenEnd`, `Preloader`
*/
native PreloadGenClear  takes nothing returns nothing

/**
Starts an internal timer for preloads. The timer will be used and recorded by PreloadGenEnd. The timer represents the wall clock time (in seconds) spent between the calls PreloadStart() and PreloadGenEnd().

This function does not clear the previous buffer.

The recorded time will be output as `call PreloadEnd( 0.123 )` in the saved preload file.

@note See: `Preload`, `PreloadEnd`, `PreloadStart`, `PreloadRefresh`, `PreloadEndEx`, `PreloadGenClear`, `PreloadGenStart`, `PreloadGenEnd`, `Preloader`
*/
native PreloadGenStart  takes nothing returns nothing

/**
Writes the current Preload buffer to specified file.
The first and final preload directives are `call PreloadStart()` and `call PreloadEnd( realTime )`. The value represents the time in seconds between the calls `PreloadStart` and `PreloadGenEnd`. There's no way to get this value with the API.

Does not clear the buffer or timer after flushing. The file is overwritten. It's possible to specify subfolders: "myMapFolder/file.txt". Reforged: Any other tricks such as relative paths, UNC or drive letters will not write any files. Classic: possible to write to any path (verify?)

**Example preload file:*

	function PreloadFiles takes nothing returns nothing

		call PreloadStart()
		call Preload( "units\\human\\Knight\\Knight.mdx" )
		call PreloadEnd( 2.5 )

	endfunction

@param filename The filepath to be written to. Max length for filename is 259 characters (see: Windows MAX_PATH).

@note Before Reforged (which version?) you needed to enable "Allow Local Files" in registry.

@note **Save Path:**

Reforged: `%USERPROFILE%\Documents\Warcraft III\CustomMapData\`

Classic: ?

@note See: `Preload`, `PreloadEnd`, `PreloadStart`, `PreloadRefresh`, `PreloadEndEx`, `PreloadGenClear`, `PreloadGenStart`, `Preloader`
*/
native PreloadGenEnd    takes string filename returns nothing

/**
Runs the filename as a preload script, only if the filename has an extension. For Jass, the capabilities are very restricted.

**Example (from blizzard.j)**:

    if (doPreload) then
            call Preloader( "scripts\\HumanMelee.pld" )
    endif

@param filename The file to execute.

@note There're no restrictions for Lua code if you add it to Preload files (which are supposed to be in Jass), that's only possible with [dirty hacks or manual editing](https://www.hiveworkshop.com/threads/blizzards-hidden-jass2lua-transpiler.337281/). If the map runs in Lua mode, the Jass code is transpiled using Jass2Lua before execution.

@note On pre-Reforged (version?) this only works if you have enabled the usage of local files in your registry.
The registry key is `HKEY_CURRENT_USER\Software\Blizzard Entertainment\Warcraft III\Allow Local Files\`

@note Here are some ways to get the data out of the preload file into your map:
To store multiple integers you can use `SetPlayerTechMaxAllowed` to have a good
2d-array. Read via `GetPlayerTechMaxAllowed`.

For strings `SetPlayerName` is suited. To read use `GetPlayerName`.

Inside the preload script you can also use `ExecuteFunc` to call your map-defined
functions and interleave the preload script with your functions.

@note If you use `Preloader` to load some values into your map, these values
are very likely to be different for each player (since the player might not 
even have local files enabled), so treat them as async values.

@note Also see the documentation of `Preload` to see how to properly get the data
into the preload script.

@bug 1.33.0 and above: Due to aggressive file caching by the game, the preload file is only loaded and read once.
This means, updates to the saved preload file cannot be reloaded and old contents will be executed.

@note See: `Preload`, `PreloadEnd`, `PreloadStart`, `PreloadRefresh`, `PreloadEndEx`, `PreloadGenClear`, `PreloadGenStart`, `PreloadGenEnd`
*/
native Preloader        takes string filename returns nothing

// Automation Test

/**
@patch 1.29
*/
native AutomationSetTestType                    takes string testType returns nothing

/**
@patch 1.29
*/
native AutomationTestStart                      takes string testName returns nothing

/**
@patch 1.30
*/
native AutomationTestEnd                        takes nothing returns nothing

/**
@patch 1.30
*/
native AutomationTestingFinished                takes nothing returns nothing

/**
"They do nothing of interest, just a compatibility thing" - MindWorX (Blizzard Developer), on [The Hive Discord](https://discord.com/channels/178569180625240064/311662737015046144/572101913349193738)

@patch 1.30
*/
native RequestExtraIntegerData                     takes integer dataType, player whichPlayer, string param1, string param2, boolean param3, integer param4, integer param5, integer param6 returns integer

/**
According to MindWorX (Blizzard Developer): //They do nothing of interest //Just a compatibility thing

@patch 1.30
*/
native RequestExtraBooleanData                     takes integer dataType, player whichPlayer, string param1, string param2, boolean param3, integer param4, integer param5, integer param6 returns boolean

/**
According to MindWorX (Blizzard Developer): //They do nothing of interest //Just a compatibility thing

@patch 1.30
*/
native RequestExtraStringData                      takes integer dataType, player whichPlayer, string param1, string param2, boolean param3, integer param4, integer param5, integer param6 returns string

/**
According to MindWorX (Blizzard Developer): //They do nothing of interest //Just a compatibility thing

@patch 1.30
*/
native RequestExtraRealData                        takes integer dataType, player whichPlayer, string param1, string param2, boolean param3, integer param4, integer param5, integer param6 returns real

