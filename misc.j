
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
Adds a string to the preload buffer.

@param filename The string to be added to the buffer.
Should probably not be named `filename`.

@note The data will be put into the file like this:
````
    call Preload("<data here>")
````
So to put any executable code in the file you have to close the string and then
put your code onto a new line, like this:
````
    call Preload("\") \n call your_stuff_here()")
````

@note Also see the documentation for `Preloader` for more info on the generated files.
*/
native Preload          takes string filename returns nothing

native PreloadEnd       takes real timeout returns nothing



native PreloadStart     takes nothing returns nothing

native PreloadRefresh   takes nothing returns nothing

native PreloadEndEx     takes nothing returns nothing



/**
Clears the preload buffer.
*/
native PreloadGenClear  takes nothing returns nothing

/**
Starts a new timer. In the generated file from `PreloadGenEnd` will be a line
like `call PreloadEnd( 0.123 )`. The argument to `PreloadEnd` is the time elapsed
between the calls of `PreloadGenStart` and `PreloadGenEnd`.
*/
native PreloadGenStart  takes nothing returns nothing

/**
Writes the preload buffer to the specific file.

@param filename The filepath to be written to.

@note In earlier versions you could give any path to write to, but
nowadays they will be put into your `game-folder/CustomMapData/`.
*/
native PreloadGenEnd    takes string filename returns nothing

/**
Executes the preload-file.

@param filename The file to execute.

@note This only works if you have enable the usage of local files in your registry.
The registry key is `HKEY_CURRENT_USER\\Software\\Blizzard Entertainment\\Warcraft III\\Allow Local Files\`

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

*/
native Preloader        takes string filename returns nothing

// Automation Test
native AutomationSetTestType                    takes string testType returns nothing
native AutomationTestStart                      takes string testName returns nothing
native AutomationTestEnd                        takes nothing returns nothing
native AutomationTestingFinished                takes nothing returns nothing

native RequestExtraIntegerData                     takes integer dataType, player whichPlayer, string param1, string param2, boolean param3, integer param4, integer param5, integer param6 returns integer
native RequestExtraBooleanData                     takes integer dataType, player whichPlayer, string param1, string param2, boolean param3, integer param4, integer param5, integer param6 returns boolean
native RequestExtraStringData                      takes integer dataType, player whichPlayer, string param1, string param2, boolean param3, integer param4, integer param5, integer param6 returns string
native RequestExtraRealData                        takes integer dataType, player whichPlayer, string param1, string param2, boolean param3, integer param4, integer param5, integer param6 returns real

