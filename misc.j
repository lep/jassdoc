
/**
Applies the specified cheat, but only if the game is single player. There are a few cheats that can be toggled on or off. If the cheat is enabled, entering the cheat again will disable it. If the cheat is disabled, entering the cheat will enable it again.
Upon entering, the text "Cheat Enabled!" will be displayed.

@param cheatStr
The cheat to enter. 

@note For a list of all cheats see <http://classic.battle.net/war3/cheatcodes.shtml>.
*/
native Cheat            takes string cheatStr returns nothing

native IsNoVictoryCheat takes nothing returns boolean

native IsNoDefeatCheat  takes nothing returns boolean



native Preload          takes string filename returns nothing

native PreloadEnd       takes real timeout returns nothing



native PreloadStart     takes nothing returns nothing

native PreloadRefresh   takes nothing returns nothing

native PreloadEndEx     takes nothing returns nothing



native PreloadGenClear  takes nothing returns nothing

native PreloadGenStart  takes nothing returns nothing

native PreloadGenEnd    takes string filename returns nothing

native Preloader        takes string filename returns nothing
